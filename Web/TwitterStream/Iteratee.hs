module Web.TwitterStream.Iteratee
  ( driver
  , status
  ) where

import Prelude hiding (catch)
import Control.Monad.IO.Class
import Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_)
import Control.Exception (catch, SomeException(..))
import Control.Monad (liftM)

-- for the JSON instance:
import Web.Twitter.Types.Import ()
import Web.Twitter.Types (Status(..))
import qualified Data.Iteratee as I
import qualified Data.Iteratee.Char as IC
import qualified Network.Curl as Curl
import qualified Text.JSON as Json

import Web.TwitterStream.Types

url :: Stream -> [Char]
url Sample   = "http://stream.twitter.com/1/statuses/sample.json"
url Firehose = "http://stream.twitter.com/1/statuses/firehose.json"

driver :: Auth -> Stream -> I.IterateeG [] Char IO a -> IO a
driver (BasicAuth username password) stream iterStart = Curl.withCurlDo $ do
  iterMV <- newMVar iterStart
  h <- Curl.initialize
  Curl.setopts h
    [ Curl.CurlFailOnError   $ True
    , Curl.CurlURL           $ url stream
    , Curl.CurlUserPwd       $ username ++ ":" ++ password
    , Curl.CurlHttpAuth      $ [Curl.HttpAuthAny]
    , Curl.CurlWriteFunction $ Curl.callbackWriter (iterChunk iterMV) ]

  Curl.perform h
  I.run =<< readMVar iterMV
  where
    iterChunk iterMV chunk = modifyMVar_ iterMV $ \iter -> do
      cont <- I.runIter iter (I.Chunk $ chunk)
      case cont of
        -- TODO - actually handle the cases here.
        I.Done x _ -> return $ I.IterateeG $ return . I.Done x
        I.Cont next _ -> return next

status :: (Monad m, MonadIO m) => I.IterateeG [] Char m ParsedStatus
status = do
  someLine <- IC.line
  case someLine of
    Left _  -> return EOF
    Right l -> liftIO $ safeDecodeStatusIO l

safeDecodeStatusIO line = catch (decodeStatus line) nothing
  where 
    decodeStatus line = do
      case Json.decode line of
        Json.Ok status' -> return $ Ok line status'
        Json.Error _    -> return $ Error line

    nothing :: SomeException -> IO ParsedStatus
    nothing = const $ return $ Error line
