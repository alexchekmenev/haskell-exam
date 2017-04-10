module Model where

import qualified Data.IntMap     as IntMap
import           Data.Time.Clock
import           Data.Typeable

data Message = Message {
  sender :: String,
  text   :: String,
  time   :: UTCTime
} deriving (Show, Typeable)

data MessagesDb = MessagesDb { allMessages :: IntMap.IntMap Message }
  deriving (Typeable)

data ChatContext = ChatContext {
  login      :: String,
  activeChat :: String
} deriving (Show, Typeable)

data ChatContextDb = ChatContextDb { chatContext :: ChatContext }
  deriving (Typeable)
