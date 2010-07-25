module Web.TwitterStream 
  ( Status(..)
  , foreachStatus
  , foreachStatusInFile
  , module Web.TwitterStream.Types
  , module Web.TwitterStream.Iteratee
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Iteratee as I

import Web.TwitterStream.Types
import Web.TwitterStream.Iteratee
import Web.Twitter.Types (Status(..))

foreachStatus :: Auth -> Stream -> (ParsedStatus -> IO a) -> IO ()
foreachStatus auth stream m = driver auth stream (iterIO m)

foreachStatusInFile :: FilePath -> (ParsedStatus -> IO a) -> IO ()
foreachStatusInFile path m = I.fileDriver (iterIO m) path

iterIO m = do
  st <- status
  liftIO $ m st
  case st of
    EOF -> return ()
    _ -> iterIO m
