module Web.TwitterStream.Types
  ( Auth(..)
  , Stream(..)
  , ParsedStatus(..)
  ) where

import Web.Twitter.Types (Status(..))

data Auth = BasicAuth String String  -- username, password
data Stream = Sample | Firehose
data ParsedStatus = Ok String Status | Error String | EOF
