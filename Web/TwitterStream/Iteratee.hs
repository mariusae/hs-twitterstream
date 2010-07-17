module Web.TwitterStream.Iteratee
  ( driver
  , status
  ) where

import Prelude hiding (catch)
import Control.Monad.IO.Class
import Control.Concurrent.MVar (newMVar, modifyMVar_)
import Control.Exception (catch, SomeException(..))

-- for the JSON instance:
import qualified Web.Twitter.Types.Import as TwitterTypes
import qualified Data.Iteratee as Iter
import Data.Iteratee.Char (line)
import qualified Network.Curl as Curl
import Web.Twitter.Types (Status(..))
import Text.JSON (decode, Result(..))

import Web.TwitterStream.Types

url Sample   = "http://stream.twitter.com/1/statuses/sample.json"
url Firehose = "http://stream.twitter.com/1/statuses/firehose.json"

driver :: Auth -> Stream -> Iter.IterateeG [] Char IO a -> IO ()
driver (BasicAuth username password) stream iter' = Curl.withCurlDo $ do
  iter <- newMVar iter'

  let recvChunk chunk = modifyMVar_ iter $ \iter -> do
        cont <- Iter.runIter iter (Iter.Chunk $ chunk)
        case cont of
          -- TODO - actually handle the cases here.
          Iter.Done x _ -> return iter
          Iter.Cont next _ -> return next

  h <- Curl.initialize
  Curl.setopts h
    [ Curl.CurlFailOnError True
    , Curl.CurlURL $ url stream
    , Curl.CurlUserPwd $ username ++ ":" ++ password
    , Curl.CurlHttpAuth [Curl.HttpAuthAny]
    , Curl.CurlWriteFunction $ Curl.callbackWriter recvChunk
    ]

  Curl.perform h
  return ()

status :: (Monad m, MonadIO m) => Iter.IterateeG [] Char m (Maybe Status)
status = do
  l <- line
  case l of
    Left _ -> return Nothing
    Right l ->
      return =<< liftIO $ 
        catch (decodeStatus l)
              (const $ return Nothing :: SomeException -> IO (Maybe Status))

  where 
    decodeStatus line = do
      case decode line of
        Ok status -> return $ Just status
        Error _   -> return Nothing
