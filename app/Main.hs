{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Dao
import           Data.Acid                            (openLocalState, query,
                                                       update)
import           Data.Acid.Abstract                   (AcidState)
import           Data.IntMap                          (empty)
import           Data.Text.Lazy                       (pack)
import           Data.Time.Clock                      (getCurrentTime)
import           Data.Time.Format
import           Model
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Web.Scotty                           as W

sleepMs n = threadDelay (n * 1000)

printMessage :: Message -> IO ()
printMessage m = putStrLn $ "["++ sender m ++ "]: " ++ text m

client :: AcidState MessagesDb -> AcidState ChatContextDb -> IO ()
client msgDb ctxDb = do
  ctx <- getContext ctxDb
  print ctx
  if login ctx == "" && activeChat ctx == "" then do
    putStrLn "Enter your login or `:q` to quit"
    ln <- getLine
    when (ln /= ":q") (do
      putStrLn $ "Your login: `" ++ ln ++ "`"
      updateContext ctxDb $ ChatContext ln ""
      client msgDb ctxDb)
    when (ln == ":q") (do
      putStrLn "Bye! Press ^C to exit"
      return ())
  else
    if activeChat ctx == "" then do
      putStrLn "Enter your recepient login or `:q` to choose new login"
      rc <- getLine
      when (rc /= ":q") (do
        putStrLn $ "Your chat with `" ++ rc ++ "` (`:q` to choose another chat)"
        putStrLn "..."
        msgs <- lastKMessages msgDb (login ctx) rc 10
        -- mapM_ printMessage msgs
        print msgs
        updateContext ctxDb $ ChatContext (login ctx) rc
        client msgDb ctxDb)
      when (rc == ":q") (do
        putStrLn "You are logged out"
        updateContext ctxDb $ ChatContext "" ""
        client msgDb ctxDb)
    else do
      msgs <- lastKMessages msgDb (login ctx) (activeChat ctx) 10
      print msgs
      msg <- getLine
      when (msg /= ":q") (do
        putStrLn $ "["++ login ctx ++"]: " ++ msg
        client msgDb ctxDb)
      when (msg == ":q") (do
        updateContext ctxDb $ ChatContext (login ctx) ""
        client msgDb ctxDb)

main :: IO ()
main = do
  state <- openLocalState (MessagesDb empty)
  chatContext <- openLocalState (ChatContextDb $ ChatContext "" "")
  -- updateContext chatContext $ ChatContext "" ""
  forkIO (do
    sleepMs 3
    client state chatContext)
  W.scotty 3000 $ do
    -- middleware logStdoutDev
    W.get "/" $ do
      a <- liftIO $ gallMessages state
      W.text $ pack $ show a
    W.get "/receive/:from/:text" $ do
      msgFrom :: String <- W.param "from"
      msgText :: String <- W.param "text"
      timestamp <- lift getCurrentTime
      liftIO $ saveMessage state $ Message msgFrom msgText timestamp
      c <- liftIO $ getContext chatContext
      if msgFrom == activeChat c && msgFrom /= ""
        then do
          lift $ putStrLn $ "[" ++ msgFrom ++ "]: " ++ msgText
          W.text "Ok"
        else
          W.text "Ok"
-- sendMessage :: AcidState MessagesDb -> String -> IO ()

getContext :: AcidState ChatContextDb -> IO ChatContext
getContext state = query state GetChatContext

updateContext :: AcidState ChatContextDb -> ChatContext -> IO ChatContext
updateContext state newCtx = do
  void $ update state $ UpdateChatContext newCtx
  return newCtx

saveMessage :: AcidState MessagesDb -> Message -> IO ()
saveMessage state msg = update state $ AddMessage msg

gallMessages :: AcidState MessagesDb -> IO [Message]
gallMessages state = query state GetMessages

lastKMessages :: AcidState MessagesDb -> String -> String -> Int -> IO [Message]
lastKMessages state from to k = query state $ LastKChatMessages from to k
