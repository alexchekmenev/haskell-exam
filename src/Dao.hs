{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Dao where
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (modify)
import           Data.Acid            (Query, Update, makeAcidic)
import qualified Data.IntMap          as IntMap
import           Data.List            (sortBy)
import           Data.Ord             (comparing)
import           Data.SafeCopy        (base, deriveSafeCopy)
import           Model

deriveSafeCopy 0 'base ''Message
deriveSafeCopy 0 'base ''MessagesDb

getMessages :: Query MessagesDb [Message]
getMessages = sortBy (comparing time) . IntMap.elems . allMessages <$> ask

lastKChatMessages :: String -> Int -> Query MessagesDb [Message]
lastKChatMessages to k =
  reverse .
  take k .
  reverse .
  sortBy (comparing time) .
  filter (\m -> recepient m == to || sender m == to) .
  IntMap.elems . allMessages <$> ask

addMessage :: Message -> Update MessagesDb ()
addMessage message = modify go
  where
    go (MessagesDb db) = MessagesDb $
      case IntMap.maxViewWithKey db of
        Just ((max, _), _) -> IntMap.insert (max + 1) message db
        Nothing            -> IntMap.singleton 1 message

makeAcidic ''MessagesDb ['addMessage, 'getMessages, 'lastKChatMessages]

deriveSafeCopy 0 'base ''ChatContext
deriveSafeCopy 0 'base ''ChatContextDb

getChatContext :: Query ChatContextDb ChatContext
getChatContext = chatContext <$> ask

updateChatContext :: ChatContext -> Update ChatContextDb ()
updateChatContext ctx = modify go
  where
    go (ChatContextDb db) = ChatContextDb $ ChatContext (login ctx) (activeChat ctx)

makeAcidic ''ChatContextDb ['getChatContext, 'updateChatContext]
