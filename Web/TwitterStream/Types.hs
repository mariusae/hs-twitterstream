module Web.TwitterStream.Types
  ( Auth(..)
  , Stream(..)
  ) where

data Auth = BasicAuth String String  -- username, password
data Stream = Sample | Firehose
