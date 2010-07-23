module Main where

import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad (forever, when)
import System (getArgs)
import Web.Twitter.Types (Status(..))

import qualified Web.TwitterStream as Stream
import qualified Data.Iteratee as Iter

main = do
  u : p : _ <- getArgs

  Stream.driver (Stream.BasicAuth u p) Stream.Sample $ forever $ do
    s <- Stream.status
    when (isJust s) $ do
      liftIO $ f (fromJust s)

  where
    f tweet = putStrLn $ statusId tweet
