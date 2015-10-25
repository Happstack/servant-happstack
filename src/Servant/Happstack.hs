{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
-- #if !MIN_VERSION_base(4,8,0)
-- {-# LANGUAGE OverlappingInstances #-}
-- #endif
module Servant.Happstack where


import           Control.Monad.Trans         (liftIO)
import           Control.Monad.Trans.Except  (ExceptT)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Map                    as M
import           Data.Maybe                  (mapMaybe, fromMaybe)
import           Data.Proxy                  (Proxy(..))
import           Data.String                 (fromString)
import           Data.String.Conversions     (cs, (<>), ConvertibleStrings)
import           Data.Text                   (Text)
import           GHC.TypeLits                (KnownSymbol, symbolVal)
import           Servant.API                 ((:<|>) (..), (:>), Capture,
                                              Delete, Get, Header,
                                              {- IsSecure(..), -} Patch, Post, Put,
                                              QueryFlag, QueryParam, QueryParams,
                                              Raw, {- RemoteHost, -} ReqBody{- , Vault -})
import           Servant.API.ContentTypes    (AcceptHeader(..), AllCTRender(..))
import           Control.Monad.Trans.Except         (ExceptT, runExceptT)
import Servant.Common.Text (FromText, fromText)
import Servant.Happstack.Internal.ServantErr
import Servant.Happstack.Internal.RoutingApplication
import Servant.Happstack.Internal.Router
import Happstack.Server

class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> IO (RouteResult (Server layout)) -> Router

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

  route Proxy server = choice (route pa (extractL <$> server))
                              (route pb (extractR <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

captured :: FromText a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = fromText

-- | If you use 'Capture' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by the 'Capture'.
-- This lets servant worry about getting it from the URL and turning
-- it into a value of the type you specify.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = getBook
-- >   where getBook :: Text -> ExceptT ServantErr IO Book
-- >         getBook isbn = ...
instance (KnownSymbol capture, FromText a, HasServer sublayout)
      => HasServer (Capture capture a :> sublayout) where

  type ServerT (Capture capture a :> sublayout) m =
     a -> ServerT sublayout m

  route Proxy subserver =
    DynamicRouter $ \ first ->
      route (Proxy :: Proxy sublayout)
            (case captured captureProxy first of
               Nothing  -> return $ failWith NotFound
               Just v   -> feedTo subserver v)
    where captureProxy = Proxy :: Proxy (Capture capture a)

allowedMethodHead :: Method -> Request -> Bool
allowedMethodHead method request = method == GET && rqMethod request == HEAD

allowedMethod :: Method -> Request -> Bool
allowedMethod method request = allowedMethodHead method request || (rqMethod request == method)

processMethodRouter :: forall a. ConvertibleStrings a B.ByteString
                    => Maybe (a, BL.ByteString) -> Status -> Method
                    -> Maybe [(B.ByteString, B.ByteString)]
                    -> Request -> RouteResult Response
processMethodRouter handleA status method headers request = case handleA of
  Nothing -> failWith UnsupportedMediaType
  Just (contentT, body) -> succeedWith $ setStatus status $ addHeaders headers $ toResponseBS (cs contentT) bdy
    where
      bdy = if allowedMethodHead method request then "" else body
      addHeaders Nothing res = res
      addHeaders (Just hdrs) res = foldr (uncurry setHeaderBS) res hdrs

methodRouter :: (AllCTRender ctypes a)
             => Method -> Proxy ctypes -> Status
             -> IO (RouteResult (ExceptT ServantErr IO a))
             -> Router
methodRouter method proxy status action = LeafRouter route'
  where
    route' :: Request -> (RouteResult Response -> IO Response) -> IO Response
    route' request respond

      | pathIsEmpty request && allowedMethod method request =
          runAction action respond $ \ output -> do
            let accH = fromMaybe ct_wildcard $ fmap (head . hValue) $ M.lookup "Accept" $ rqHeaders request
                handleA = handleAcceptH proxy (AcceptHeader accH) output
            processMethodRouter handleA status method Nothing request

      | pathIsEmpty request && rqMethod request /= method =
          respond $ failWith WrongMethod

      | otherwise = respond $ failWith NotFound

methodRouterEmpty :: Method
                  -> IO (RouteResult (ExceptT ServantErr IO ()))
                  -> Router
methodRouterEmpty method action = LeafRouter route'
  where
    route' request respond
      | pathIsEmpty request && allowedMethod method request = do
          runAction action respond $ \ () ->
            succeedWith $ responseLBS noContent204 [] ""
      | pathIsEmpty request && rqMethod request /= method =
          respond $ failWith WrongMethod
      | otherwise = respond $ failWith NotFound

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
-- #if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
-- #endif
         ( AllCTRender ctypes a ) => HasServer (Get ctypes a) where

  type ServerT (Get ctypes a) m = m a

  route Proxy = methodRouter GET (Proxy :: Proxy ctypes) (Status 200)

-- '()' ==> 204 No Content
instance
-- #if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
-- #endif
          HasServer (Get ctypes ()) where

  type ServerT (Get ctypes ()) m = m ()

  route Proxy = methodRouterEmpty GET

pathIsEmpty :: Request -> Bool
pathIsEmpty = go . rqPaths
  where go []   = True
        go [""] = True
        go _    = False

ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*" -- Because CPP

responseLBS :: Status -> [(B.ByteString, B.ByteString)] -> BL.ByteString -> Response
responseLBS status headers bdy =
    foldr (uncurry setHeaderBS) ((toResponse bdy) { rsCode = unStatus status }) headers

noContent204 = Status 204
notFound404 = Status 404

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
serve p server = toServerPart (runRouter (route p (return (RR (Right server)))))
    where
      toServerPart :: RoutingApplication -> ServerPart Response
      toServerPart ra =
          do req <- askRq
             res <- liftIO $ ra req respond
             pure res
      respond :: RouteResult Response -> IO Response
      respond (RR rr) =
          case rr of
            (Left NotFound) ->
                pure $ responseLBS notFound404 [] "not found"
            (Right response) ->
                pure response
