{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Except        ( MonadError, ExceptT, Except, runExceptT, withExceptT  )
import Control.Monad.IO.Class      ( MonadIO     )
import Control.Monad.Reader        ( MonadReader, ReaderT, runReaderT   )
-- import Control.Monad.Trans.Either  ( EitherT, runEitherT )
import Control.Monad.Trans.Either
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Happstack.Server (Response, ServerPart, simpleHTTP, nullConf)
import Servant.API
import Servant.Happstack
import Servant.Happstack.Internal.Enter
import Servant.Happstack.Internal.ServantErr
import Todo.Type.API
import Todo.Type.UUID
import Todo.Type.User
import Todo.Type.Todo
import Todo.Type.Error
-- import Todo.DB.Todo
-- import Todo.DB.User
import Control.Concurrent.STM

data Config = Config {
      port  :: Int
--    , tododb :: TVar TodoDB
--    , userdb :: TVar UserDB
    }

------------------------------------------------------------------------------
-- | Core Todo Type
newtype TodoApp a = TodoApp {
    runTodo ::  ReaderT Config (ExceptT Error IO) a
  } deriving ( MonadIO, MonadReader Config
             , Applicative, Monad, Functor, MonadError Error )
------------------------------------------------------------------------------
-- | runApp - helper for transformer stack evaluation
runApp :: Config -> TodoApp a -> IO (Either Error a)
runApp config = runExceptT . flip runReaderT config . runTodo

type SimpleAPI = Get '[JSON] Text

api :: Proxy SimpleAPI
api = Proxy

todoEndpoints :: ServerT SimpleAPI TodoApp
todoEndpoints = pure "This is it!"

app :: Config -> ServerPart Response
app cfg = serve api server
    where
      server :: Server SimpleAPI
      server = enter todoToExcept todoEndpoints

      todoToExcept :: TodoApp :~> ExceptT ServantErr IO
      todoToExcept = Nat $ withExceptT errorToServantErr -- flip bimapExceptT id errorToServantErr
                         . flip runReaderT cfg . runTodo

      errorToServantErr :: Error -> ServantErr
      errorToServantErr = const err500

main :: IO ()
main = simpleHTTP nullConf (app (Config 8000))
