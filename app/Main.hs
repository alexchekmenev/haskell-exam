{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Acid                 (closeAcidState, openLocalState,
                                            query, update)
import           Data.Acid.Abstract        (AcidState)
import           Data.IntMap               (empty)
import           Data.Text.Lazy            (Text, pack, unpack)
import           Data.Time.Clock           (getCurrentTime)
import           Model
-- import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Control.Exception         (bracket)
import qualified Web.Scotty                as W

{-|
  Helper function is wrapper for threadDelay
-}
sleepMs :: Int -> IO ()
sleepMs n = threadDelay (n * 1000)

{-|
  Print Message to stdin
-}
printMessage :: Message -> IO ()
printMessage m = putStrLn $ "["++ unpack (sender m) ++ "]: " ++ unpack (text m)

{-|
  Function is the implementation of the Finite-state machine of the client env
-}
client :: AcidState MessagesDb -> AcidState ChatContextDb -> IO ()
client msgDb ctxDb = do
  ctx <- getContext ctxDb
  -- print ctx
  if login ctx == "" && activeChat ctx == "" then do
    putStrLn "Enter your login or `:q` to quit"
    ln <- getLine
    if ln /= ":q" then do
      putStrLn $ "Your login: `" ++ ln ++ "`"
      updateContext ctxDb $ ChatContext (pack ln) ""
      client msgDb ctxDb
    else do
      putStrLn "Bye! Press ^C to exit"
      return ()
  else
    if activeChat ctx == "" then do
      putStrLn "Enter your recepient login or `:q` to choose new login"
      rc <- getLine
      if rc /= ":q" then do
        putStrLn $ "Your chat with `" ++ rc ++ "` (`:q` to choose another chat)"
        putStrLn "..."
        msgs <- lastKMessages msgDb (pack rc) 10
        mapM_ printMessage msgs
        updateContext ctxDb $ ChatContext (login ctx) (pack rc)
        client msgDb ctxDb
      else do
        putStrLn "You are logged out"
        updateContext ctxDb $ ChatContext "" ""
        client msgDb ctxDb
    else do
      msg <- getLine
      if msg /= ":q" then do
        timestamp <- getCurrentTime
        saveMessage msgDb $ Message (login ctx) (activeChat ctx) (pack msg) timestamp
        putStrLn $ "["++ unpack (login ctx) ++"]: " ++ msg
        client msgDb ctxDb
      else do
        updateContext ctxDb $ ChatContext (login ctx) ""
        client msgDb ctxDb

{-|
  Main Scotty server function
-}
main :: IO ()
main = bracket (do
    msgDb <- openLocalState (MessagesDb empty)
    ctxDb <- openLocalState (ChatContextDb $ ChatContext "" "")
    -- updateContext chatContext $ ChatContext "" ""
    return (msgDb, ctxDb))
    (\(msgDb, ctxDb) -> do
      putStrLn "Releasing..."
      closeAcidState msgDb
      closeAcidState ctxDb)
    (\(state, ctxDb) -> do
      _ <- forkIO $ do
        sleepMs 3
        client state ctxDb
      W.scotty 3000 $ do
        -- middleware logStdoutDev
        W.get "/" $ do
          a <- liftIO $ getAllMessages state
          W.text $ pack $ show a
        W.get "/receive/:from/:text" $ do
          msgFrom :: Text <- W.param "from"
          msgText :: Text <- W.param "text"
          timestamp <- lift getCurrentTime
          c <- liftIO $ getContext ctxDb
          liftIO $ saveMessage state $ Message msgFrom (login c) msgText timestamp
          if msgFrom == activeChat c && msgFrom /= ""
            then do
              lift $ putStrLn $ "[" ++ unpack msgFrom ++ "]: " ++ unpack msgText
              W.text "Ok"
            else
              W.text "Ok")

{-|
  Gets context from its acid-state
-}
getContext :: AcidState ChatContextDb -> IO ChatContext
getContext state = query state GetChatContext

{-|
  Updates context stored in its acid-state
-}
updateContext :: AcidState ChatContextDb -> ChatContext -> IO ()
updateContext state newCtx = update state $ UpdateChatContext newCtx

{-|
  Saves Message to its acid-state
-}
saveMessage :: AcidState MessagesDb -> Message -> IO ()
saveMessage state msg = update state $ AddMessage msg

{-|
  Gets all stored messages
-}
getAllMessages :: AcidState MessagesDb -> IO [Message]
getAllMessages state = query state GetMessages

{-|
  Gets from acid-state last k messages from the dialog with to
-}
lastKMessages :: AcidState MessagesDb -> Text -> Int -> IO [Message]
lastKMessages state to k = query state $ LastKChatMessages to k
