{-# LANGUAGE LambdaCase #-}

module Main where

import ClassyPrelude
import Network.RTM.Slack.Connection
import Control.Lens ( (^.) )

main :: IO ()
main = do
  tok <- fromMaybe (error "Provide token as first argument") . headMay <$> getArgs
  withConnection tok $ \conn ->
    getNotifications conn >>= getEvent conn
  where
    getEvent conn ch = do
      atomically (readTChan ch) >>= \case
        Connected s ->
          putStrLn $ "Connected as " <> s ^. slackSelf.selfName
        Disconnected r ->
          putStrLn $ "Disconnected: " <> pack (show r)
        SlackEvent (Message cid _ msg _ _ _) _ -> do
          void $ postMessage conn cid msg
          putStrLn "Received message"
        SlackEvent _ _ ->
          putStrLn "Event"
      getEvent conn ch
