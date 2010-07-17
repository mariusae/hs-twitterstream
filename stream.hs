module Main where

import Control.Monad.IO.Class
import System (getArgs)
import Web.Twitter.Types (Status(..))

import qualified Web.TwitterStream as Stream
import qualified Data.Iteratee as Iter

main = do
  u : p : _ <- getArgs

  Stream.driver (Stream.BasicAuth u p) Stream.Sample tweets

  where
    tweets = do
      s <- Stream.status
      case s of
        Just s -> (liftIO $ f s) >> tweets
        Nothing -> tweets

    f tweet = putStrLn $ statusText tweet
