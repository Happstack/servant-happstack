{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances #-}
#endif
module Servant.Happstack where


import           Control.Arrow               (first)
import           Control.Monad.Trans         (liftIO)
import           Control.Monad.Trans.Except  (ExceptT)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Map                    as M
import           Data.Maybe                  (mapMaybe, fromMaybe)
import           Data.Proxy                  (Proxy(..))
import           Data.String                 (fromString)
import           Data.String.Conversions     (cs, (<>), ConvertibleStrings)
import           Data.CaseInsensitive        (mk, original)
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           GHC.TypeLits                (KnownSymbol, symbolVal)
import           Servant.API                 ((:<|>) (..), (:>), Capture,
                                              Delete, Get, Header,
                                              IsSecure(..), Patch, Post, Put,
                                              QueryFlag, QueryParam, QueryParams,
                                              Raw, RemoteHost, ReqBody{- , Vault -})
import           Servant.API.ContentTypes    (AcceptHeader(..), AllCTRender(..), AllCTUnrender, AllMime, canHandleAcceptH, handleCTypeH)
import           Servant.API.ResponseHeaders (GetHeaders, Headers, getHeaders, getResponse)
import           Control.Monad.Trans.Except         (ExceptT, runExceptT)
import           Network.Socket               (SockAddr(..))
import qualified Network.HTTP.Types as HTTP
import           Network.HTTP.Types (HeaderName, Status(..), created201, hAccept, hContentType, ok200, noContent204, parseQueryText)
import Servant.Happstack.Internal.ServantErr
import Servant.Happstack.Internal.RoutingApplication
import Servant.Happstack.Internal.Router
import Servant.Happstack.Internal.ServantErr
import Happstack.Server                   hiding (Headers, getResponse)
import Happstack.Server.Internal.Monads   (WebT, FilterFun, unFilterFun, runServerPartT, ununWebT)
import           Web.HttpApiData          (FromHttpApiData)
import           Web.HttpApiData.Internal (parseUrlPieceMaybe, parseHeaderMaybe, parseQueryParamMaybe)

class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> Delayed (Server layout) -> Router

type Server layout = ServerT layout (ExceptT ServantErr IO)

-- * Instances

-- | A server for @a ':<|>' b@ first tries to match the request against the route
--   represented by @a@ and if it fails tries @b@. You must provide a request
--   handler for each route.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post '[JSON] Book -- POST /books
-- >
-- > server :: Server MyApi
-- > server = listAllBooks :<|> postBook
-- >   where listAllBooks = ...
-- >         postBook book = ...
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where

  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy server = choice (route pa ((\ (a :<|> _) -> a) <$> server))
                              (route pb ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

captured :: FromHttpApiData a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = parseUrlPieceMaybe

-- | If you use 'Capture' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by the 'Capture'.
-- This lets servant worry about getting it from the URL and turning
-- it into a value of the type you specify.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromHttpApiData' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = getBook
-- >   where getBook :: Text -> ExceptT ServantErr IO Book
-- >         getBook isbn = ...
instance (KnownSymbol capture, FromHttpApiData a, HasServer sublayout)
      => HasServer (Capture capture a :> sublayout) where

  type ServerT (Capture capture a :> sublayout) m =
     a -> ServerT sublayout m

  route Proxy d =
    DynamicRouter $ \ first ->
        route (Proxy :: Proxy sublayout)
              (addCapture d $ case captured captureProxy first of
                 Nothing -> return $ Fail err404
                 Just v  -> return $ Route v
              )
    where captureProxy = Proxy :: Proxy (Capture capture a)

allowedMethodHead :: Method -> Request -> Bool
allowedMethodHead method request = method == GET && rqMethod request == HEAD

allowedMethod :: Method -> Request -> Bool
allowedMethod method request = allowedMethodHead method request || rqMethod request == method

processMethodRouter :: forall a. ConvertibleStrings a B.ByteString
                    => Maybe (a, BL.ByteString) -> Status -> Method
                    -> Maybe [(HeaderName, B.ByteString)]
                    -> Request -> RouteResult Response
processMethodRouter handleA status method headers request = case handleA of
  Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
  Just (contentT, body) -> Route $ responseLBS status hdrs bdy
    where
      bdy = if allowedMethodHead method request then "" else body
      hdrs = (hContentType, cs contentT) : (fromMaybe [] headers)

methodCheck :: Method -> Request -> IO (RouteResult ())
methodCheck method request
  | allowedMethod method request = return $ Route ()
  | otherwise                    = return $ Fail err405

acceptCheck :: (AllMime list) => Proxy list -> B.ByteString -> IO (RouteResult ())
acceptCheck proxy accH
  | canHandleAcceptH proxy (AcceptHeader accH) = return $ Route ()
  | otherwise                                  = return $ Fail err406

methodRouter :: (AllCTRender ctypes a)
             => Method -> Proxy ctypes -> Status
             -> Delayed (ExceptT ServantErr IO a)
             -> Router
methodRouter method proxy status action = LeafRouter route'
  where
--    route' :: Request -> (RouteResult Response -> IO Response) -> IO Response
    route' request respond

      | pathIsEmpty request =
          let accH = fromMaybe ct_wildcard $ lookup HTTP.hAccept $ requestHeaders request
          in runAction (action `addMethodCheck` methodCheck method request
                               `addMethodCheck` acceptCheck proxy accH
                       ) respond $ \ output -> do
               let handleA = handleAcceptH proxy (AcceptHeader accH) output
               processMethodRouter handleA status method Nothing request

      | otherwise = respond $ Fail err404

methodRouterHeaders :: (GetHeaders (Headers h v), AllCTRender ctypes v)
                    => Method -> Proxy ctypes -> Status
                    -> Delayed (ExceptT ServantErr IO (Headers h v))
                    -> Router
methodRouterHeaders method proxy status action = LeafRouter route'
  where
    route' request respond
      | pathIsEmpty request =
          let accH    = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` acceptCheck proxy  accH
                       ) respond $ \ output -> do
                let headers = getHeaders output
                    handleA = handleAcceptH proxy (AcceptHeader accH) (getResponse output)
                processMethodRouter handleA status method (Just headers) request
      | otherwise = respond $ Fail err404

methodRouterEmpty :: Method
                  -> Delayed (ExceptT ServantErr IO ())
                  -> Router
methodRouterEmpty method action = LeafRouter route'
  where
    route' request respond
      | pathIsEmpty request = do
          runAction (addMethodCheck action (methodCheck method request)) respond $ \ () ->
            Route $! responseLBS noContent204 [] ""
      | otherwise = respond $ Fail err404

-- | If you have a 'Delete' endpoint in your API,
-- the handler for this endpoint is meant to delete
-- a resource.
--
-- The code of the handler will, just like
-- for 'Servant.API.Get.Get', 'Servant.API.Post.Post' and
-- 'Servant.API.Put.Put', run in @ExceptT ServantErr IO ()@.
-- The 'Int' represents the status code and the 'String' a message
-- to be returned. You can use 'Control.Monad.Trans.Except.throwE' to
-- painlessly error out if the conditions for a successful deletion
-- are not met.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( AllCTRender ctypes a
         ) => HasServer (Delete ctypes a) where

  type ServerT (Delete ctypes a) m = m a

  route Proxy = methodRouter DELETE (Proxy :: Proxy ctypes) ok200

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         HasServer (Delete ctypes ()) where

  type ServerT (Delete ctypes ()) m = m ()

  route Proxy = methodRouterEmpty DELETE

-- Add response headers
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Delete ctypes (Headers h v)) where

  type ServerT (Delete ctypes (Headers h v)) m = m (Headers h v)

  route Proxy = methodRouterHeaders DELETE (Proxy :: Proxy ctypes) ok200

-- | When implementing the handler for a 'Get' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Post.Post'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @ExceptT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.Except.throwE'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 200). If there was no @Accept@ header or it
-- was @*\/\*@, we return encode using the first @Content-Type@ type on the
-- list.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( AllCTRender ctypes a ) => HasServer (Get ctypes a) where

  type ServerT (Get ctypes a) m = m a

  route Proxy = methodRouter GET (Proxy :: Proxy ctypes) ok200

-- '()' ==> 204 No Content
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
          HasServer (Get ctypes ()) where

  type ServerT (Get ctypes ()) m = m ()

  route Proxy = methodRouterEmpty GET

-- Add response headers
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
          ( GetHeaders (Headers h v), AllCTRender ctypes v
          ) => HasServer (Get ctypes (Headers h v)) where

  type ServerT (Get ctypes (Headers h v)) m = m (Headers h v)

  route Proxy = methodRouterHeaders GET (Proxy :: Proxy ctypes) ok200

-- | If you use 'Header' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'Header'.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
-- All it asks is for a 'FromHttpApiData' instance.
--
-- Example:
--
-- > newtype Referer = Referer Text
-- >   deriving (Eq, Show, FromHttpApiData, ToText)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- >
-- > server :: Server MyApi
-- > server = viewReferer
-- >   where viewReferer :: Referer -> ExceptT ServantErr IO referer
-- >         viewReferer referer = return referer
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout)
      => HasServer (Header sym a :> sublayout) where

  type ServerT (Header sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let mheader = parseHeaderMaybe =<< lookup str (requestHeaders request)
    in  route (Proxy :: Proxy sublayout) (passToServer subserver mheader)
    where str = fromString $ symbolVal (Proxy :: Proxy sym)

-- | When implementing the handler for a 'Post' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @ExceptT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.Except.throwE'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 201). If there was no @Accept@ header or it
-- was @*\/\*@, we return encode using the first @Content-Type@ type on the
-- list.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( AllCTRender ctypes a
         ) => HasServer (Post ctypes a) where

  type ServerT (Post ctypes a) m = m a

  route Proxy = methodRouter POST (Proxy :: Proxy ctypes) created201

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         HasServer (Post ctypes ()) where

  type ServerT (Post ctypes ()) m = m ()

  route Proxy = methodRouterEmpty POST

-- Add response headers
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Post ctypes (Headers h v)) where

  type ServerT (Post ctypes (Headers h v)) m = m (Headers h v)

  route Proxy = methodRouterHeaders POST (Proxy :: Proxy ctypes) created201

-- | When implementing the handler for a 'Put' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Post.Post', the handler code runs in the
-- @ExceptT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.Except.throwE'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 200). If there was no @Accept@ header or it
-- was @*\/\*@, we return encode using the first @Content-Type@ type on the
-- list.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( AllCTRender ctypes a) => HasServer (Put ctypes a) where

  type ServerT (Put ctypes a) m = m a

  route Proxy = methodRouter PUT (Proxy :: Proxy ctypes) ok200

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         HasServer (Put ctypes ()) where

  type ServerT (Put ctypes ()) m = m ()

  route Proxy = methodRouterEmpty PUT

-- Add response headers
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Put ctypes (Headers h v)) where

  type ServerT (Put ctypes (Headers h v)) m = m (Headers h v)

  route Proxy = methodRouterHeaders PUT (Proxy :: Proxy ctypes) ok200
{-
-- | When implementing the handler for a 'Patch' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @ExceptT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.Except.throwE'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we just require that its type has
-- a 'ToJSON' instance and servant takes care of encoding it for you,
-- yielding status code 200 along the way.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( AllCTRender ctypes a) => HasServer (Patch ctypes a) where

  type ServerT (Patch ctypes a) m = m a

  route Proxy = methodRouter methodPatch (Proxy :: Proxy ctypes) ok200

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
          HasServer (Patch ctypes ()) where

  type ServerT (Patch ctypes ()) m = m ()

  route Proxy = methodRouterEmpty methodPatch

-- Add response headers
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Patch ctypes (Headers h v)) where

  type ServerT (Patch ctypes (Headers h v)) m = m (Headers h v)

  route Proxy = methodRouterHeaders methodPatch (Proxy :: Proxy ctypes) ok200
-}
-- | If you use @'QueryParam' "author" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @'Maybe' 'Text'@.
--
-- This lets servant worry about looking it up in the query string
-- and turning it into a value of the type you specify, enclosed
-- in 'Maybe', because it may not be there and servant would then
-- hand you 'Nothing'.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromHttpApiData' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: Maybe Text -> ExceptT ServantErr IO [Book]
-- >         getBooksBy Nothing       = ...return all books...
-- >         getBooksBy (Just author) = ...return books by the given author...
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout)
      => HasServer (QueryParam sym a :> sublayout) where

  type ServerT (QueryParam sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let querytext = parseQueryText $ C.pack $ rqQuery request
        param =
          case lookup paramname querytext of
            Nothing       -> Nothing -- param absent from the query string
            Just Nothing  -> Nothing -- param present with no value -> Nothing
            Just (Just v) -> parseQueryParamMaybe v -- if present, we try to convert to
                                        -- the right type
    in route (Proxy :: Proxy sublayout) (passToServer subserver param)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)

-- | If you use @'QueryParams' "authors" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @['Text']@.
--
-- This lets servant worry about looking up 0 or more values in the query string
-- associated to @authors@ and turning each of them into a value of
-- the type you specify.
--
-- You can control how the individual values are converted from 'Text' to your type
-- by simply providing an instance of 'FromHttpApiData' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: [Text] -> ExceptT ServantErr IO [Book]
-- >         getBooksBy authors = ...return all books by these authors...
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout)
      => HasServer (QueryParams sym a :> sublayout) where

  type ServerT (QueryParams sym a :> sublayout) m =
    [a] -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let querytext = parseQueryText $ C.pack $ rqQuery request
        -- if sym is "foo", we look for query string parameters
        -- named "foo" or "foo[]" and call parseQueryParam on the
        -- corresponding values
        parameters = filter looksLikeParam querytext
        values = mapMaybe (convert . snd) parameters
    in  route (Proxy :: Proxy sublayout) (passToServer subserver values)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          looksLikeParam (name, _) = name == paramname || name == (paramname <> "[]")
          convert Nothing = Nothing
          convert (Just v) = parseQueryParamMaybe v

-- | If you use @'QueryFlag' "published"@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type 'Bool'.
--
-- Example:
--
-- > type MyApi = "books" :> QueryFlag "published" :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooks
-- >   where getBooks :: Bool -> ExceptT ServantErr IO [Book]
-- >         getBooks onlyPublished = ...return all books, or only the ones that are already published, depending on the argument...
instance (KnownSymbol sym, HasServer sublayout)
      => HasServer (QueryFlag sym :> sublayout) where

  type ServerT (QueryFlag sym :> sublayout) m =
    Bool -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let querytext = parseQueryText $ C.pack $ rqQuery request
        param = case lookup paramname querytext of
          Just Nothing  -> True  -- param is there, with no value
          Just (Just v) -> examine v -- param with a value
          Nothing       -> False -- param not in the query string
    in  route (Proxy :: Proxy sublayout) (passToServer subserver param)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          examine v | v == "true" || v == "1" || v == "" = True
                    | otherwise = False


-- | Just pass the request to the underlying application and serve its response.
--
-- Example:
--
-- > type MyApi = "images" :> Raw
-- >
-- > server :: Server MyApi
-- > server = serveDirectory "/var/www/images"
instance HasServer Raw where

  type ServerT Raw m = ServerPartT m Response

  route Proxy rawApplication = LeafRouter $ \ request respond -> do
    r <- runDelayed rawApplication
    case r of
      Route app   ->
          do r <- runExceptT $ runWebT $ runServerPartT app request
             case r of
               (Left err) -> respond $ Route $ responseServantErr err
               (Right Nothing) -> respond $ Fail err404
               (Right (Just res)) -> respond $ Route res
    where
      -- | Takes your 'WebT', if it is 'mempty' it returns 'Nothing' else it
      -- converts the value to a 'Response' and applies your filter to it.
      runWebT :: forall m b. (Functor m, ToMessage b) => WebT m b -> m (Maybe Response)
      runWebT = (fmap . fmap) appFilterToResp . ununWebT
          where
            appFilterToResp :: (Either Response b, FilterFun Response) -> Response
            appFilterToResp (e, ff) = unFilterFun ff $ either id toResponse e


-- | If you use 'ReqBody' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'ReqBody'.
-- The @Content-Type@ header is inspected, and the list provided is used to
-- attempt deserialization. If the request does not have a @Content-Type@
-- header, it is treated as @application/octet-stream@ (as specified in
-- <http://tools.ietf.org/html/rfc7231#section-3.1.1.5 RFC7231>.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
--
-- All it asks is for a 'FromJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = postBook
-- >   where postBook :: Book -> ExceptT ServantErr IO Book
-- >         postBook book = ...insert into your db...
instance ( AllCTUnrender list a, HasServer sublayout
         ) => HasServer (ReqBody list a :> sublayout) where

  type ServerT (ReqBody list a :> sublayout) m =
    a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy sublayout) (addBodyCheck subserver (bodyCheck request))
    where
      bodyCheck request = do
        -- See HTTP RFC 2616, section 7.2.1
        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
        -- See also "W3C Internet Media Type registration, consistency of use"
        -- http://www.w3.org/2001/tag/2002/0129-mime
        let contentTypeH = fromMaybe "application/octet-stream"
                         $ lookup hContentType $ requestHeaders request
        mbody <- takeRequestBody request
        case mbody of
          Nothing -> return $ FailFatal err500
          (Just (Body body)) ->
              case handleCTypeH (Proxy :: Proxy list) (cs contentTypeH) body of
                Nothing        -> return $ FailFatal err415
                Just (Left e)  -> return $ FailFatal err400 { errBody = cs e }
                Just (Right v) -> return $ Route v

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @sublayout@.
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout) where

  type ServerT (path :> sublayout) m = ServerT sublayout m

  route Proxy subserver = StaticRouter $
    M.singleton (cs (symbolVal proxyPath))
                (route (Proxy :: Proxy sublayout) subserver)
    where proxyPath = Proxy :: Proxy path


instance HasServer api => HasServer (RemoteHost :> api) where
  type ServerT (RemoteHost :> api) m = Host -> ServerT api m

  route Proxy subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) (passToServer subserver $ rqPeer req)

instance HasServer api => HasServer (IsSecure :> api) where
  type ServerT (IsSecure :> api) m = IsSecure -> ServerT api m

  route Proxy subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) (passToServer subserver $ secure req)

    where secure req = if rqSecure req then Secure else NotSecure
{-
instance HasServer api => HasServer (Vault :> api) where
  type ServerT (Vault :> api) m = Vault -> ServerT api m

  route Proxy subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) (passToServer subserver $ vault req)
-}
instance HasServer api => HasServer (HttpVersion :> api) where
  type ServerT (HttpVersion :> api) m = HttpVersion -> ServerT api m

  route Proxy subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) (passToServer subserver $ rqVersion req)

pathIsEmpty :: Request -> Bool
pathIsEmpty = go . rqPaths
  where go []   = True
        go [""] = True
        go _    = False

ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*" -- Because CPP

responseLBS :: Status -> [(HeaderName, B.ByteString)] -> BL.ByteString -> Response
responseLBS status headers bdy =
    foldr (uncurry setHeaderBS) ((toResponse bdy) { rsCode = statusCode status }) (map (first original) headers)

requestHeaders :: Request -> [(HeaderName, B.ByteString)]
requestHeaders req = concatMap (\(HeaderPair k vs) -> map (mk k,) vs) (M.elems $ rqHeaders req)

-- | 'serve' allows you to implement an API and produce a wai 'Application'.
--
-- Example:
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post '[JSON] Book -- POST /books
-- >
-- > server :: Server MyApi
-- > server = listAllBooks :<|> postBook
-- >   where listAllBooks = ...
-- >         postBook book = ...
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > app :: Application
-- > app = serve myApi server
-- >
-- > main :: IO ()
-- > main = Network.Wai.Handler.Warp.run 8080 app
--
serve :: HasServer layout => Proxy layout -> Server layout -> ServerPart Response
serve p server = toServerPart (runRouter (route p d))
    where
      d = Delayed r r r (\ _ _ -> Route server)
      r = return (Route ())
      toServerPart :: RoutingApplication -> ServerPart Response
      toServerPart ra =
          do req <- askRq
             res <- liftIO $ ra req respond
             pure res
      respond :: RouteResult Response -> IO Response
      respond (Fail err)      = pure $ responseServantErr err
      respond (FailFatal err) = pure $ responseServantErr err
      respond (Route v)       = pure v

