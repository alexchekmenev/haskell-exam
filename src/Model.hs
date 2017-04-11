{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Model where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (modify)
import           Data.Acid            (Query, Update, makeAcidic)
import           Data.IntMap          (IntMap, elems, insert, maxViewWithKey,
                                       singleton)
import           Data.List            (sortBy)
import           Data.Ord             (comparing)
import           Data.SafeCopy        (base, deriveSafeCopy)
import           Data.Text.Lazy       (Text)
import           Data.Time.Clock      (UTCTime)
import           Data.Typeable        (Typeable)
import           Prelude              hiding (max)

{-|
  Represents the Message entity
-}
data Message =
  Message { -- | sender login
            sender    :: Text,
            -- | recepient login
            recepient :: Text,
            -- | text of the Message
            text      :: Text,
            -- | time of receipt
            time      :: UTCTime
} deriving (Show, Typeable)

{-|
  Storage for messages
-}
newtype MessagesDb = MessagesDb { allMessages :: IntMap Message }
  deriving (Typeable)

{-|
  Represents context of the chat ui
-}
data ChatContext =
  ChatContext { -- | login of the current sender
                login      :: Text,
                -- | login of the current recepient
                activeChat :: Text
} deriving (Show, Typeable)

{-|
  Storage for the ui context
-}
newtype ChatContextDb = ChatContextDb { chatContext :: ChatContext }
  deriving (Typeable)

deriveSafeCopy 0 'base ''Message
deriveSafeCopy 0 'base ''MessagesDb
deriveSafeCopy 0 'base ''ChatContext
deriveSafeCopy 0 'base ''ChatContextDb

{-|
  Gets all messages from its storage
-}
getMessages :: Query MessagesDb [Message]
getMessages = sortBy (comparing time) . elems . allMessages <$> ask

{-|
  Gets lask k messages from the curtain dialog
-}
lastKChatMessages :: Text -> Int -> Query MessagesDb [Message]
lastKChatMessages to k =
  reverse .
  take k .
  sortBy (flip (comparing time)) .
  filter (\m -> recepient m == to || sender m == to) .
  elems . allMessages <$> ask

{-|
  Puts new message to the storage
-}
addMessage :: Message -> Update MessagesDb ()
addMessage message = modify go
  where
    go (MessagesDb db) = MessagesDb $
      case maxViewWithKey db of
        Just ((max, _), _) -> insert (max + 1) message db
        Nothing            -> singleton 1 message

makeAcidic ''MessagesDb ['addMessage, 'getMessages, 'lastKChatMessages]

{-|
  Gets the chat context from its storage
-}
getChatContext :: Query ChatContextDb ChatContext
getChatContext = chatContext <$> ask

{-|
  Updates the chat context & saves it to its storage
-}
updateChatContext :: ChatContext -> Update ChatContextDb ()
updateChatContext ctx = modify go
  where
    go _ = ChatContextDb $ ChatContext (login ctx) (activeChat ctx)

makeAcidic ''ChatContextDb ['getChatContext, 'updateChatContext]
