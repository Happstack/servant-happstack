{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Servant.Happstack.Internal.ServantErr where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
-- import qualified Network.HTTP.Types    as HTTP
import           Happstack.Server      (Response(..), setHeaderBS, toResponseBS)

data ServantErr = ServantErr { errHTTPCode     :: Int
                             , errReasonPhrase :: String
                             , errBody         :: LBS.ByteString
                             , errHeaders      :: [(BS.ByteString, BS.ByteString)]
                             } deriving (Show, Eq)

responseServantErr :: ServantErr -> Response
responseServantErr ServantErr{..} =
    foldr (uncurry setHeaderBS) ((toResponseBS "text/plain" errBody) { rsCode = errHTTPCode }) errHeaders
--  where
--    status = HTTP.mkStatus errHTTPCode (BS.pack errReasonPhrase)

err500 :: ServantErr
err500 = ServantErr { errHTTPCode = 500
                    , errReasonPhrase = "Internal Server Error"
                    , errBody = ""
                    , errHeaders = []
                    }
