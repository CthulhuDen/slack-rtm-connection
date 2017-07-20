{-# LANGUAGE LambdaCase #-}

module Network.RTM.Slack.Connection
  ( Notification (..)
  , Connection
  , connect
  , connectAsync
  , withConnection
  , postMessage
  , getNotifications
  , getState
  , module State
  , module ST
  ) where

import ClassyPrelude
import Web.Slack.Handle
import Network.RTM.Slack.State as State
import qualified Web.Slack.Types as ST
  hiding
    ( _slackBots, slackBots
    , _slackChannels, slackChannels
    , _slackGroups, slackGroups
    , _slackIms, slackIms
    , _slackSelf, slackSelf
    , _slackTeam, slackTeam
    , _slackUsers, slackUsers
    )
import Network.RTM.Slack.State.Update

data Notification =
    Connected SlackState                                -- ^ Contains state at the time of connection
  | Disconnected SomeException
  | SlackEvent Event SlackState                         -- ^ Contains up-to-date state

data Command =
    SendMessage ChannelId Text (TMVar SlackTimeStamp)
  | GetState (TMVar SlackState)

-- | Opaque data type representing a handle to running slack connection
data Connection = Connection (TChan Notification) (TChan Command)

data Dismissed = Dismissed
instance Show Dismissed where
  show Dismissed = "Dismissed"
instance Exception Dismissed

-- | Send a new message to the channel
postMessage :: MonadIO m
  => Connection                                         -- ^ Produced by 'connect' family of functions
  -> ChannelId                                          -- ^ Slack channel identifier
  -> Text                                               -- ^ Message text
  -> m SlackTimeStamp
postMessage (Connection _ cc) cid msg = do
  tmid <- liftIO newEmptyTMVarIO
  atomically $ writeTChan cc $ SendMessage cid msg tmid
  atomically $ readTMVar tmid

-- | Obtain a new TChan which will receive notifications from slack
getNotifications :: MonadIO m
  => Connection                                         -- ^ Produced by 'connect' family of functions
  -> m (TChan Notification)
getNotifications (Connection nc _) = atomically $ dupTChan nc

-- | Obtain current up-to-date state
getState :: MonadIO m
  => Connection                                         -- ^ Produced by 'connect' family of functions
  -> m SlackState
getState (Connection _ cc) = do
  ts <- liftIO newEmptyTMVarIO
  atomically $ writeTChan cc $ GetState ts
  atomically $ readTMVar ts

-- | Runs client in current thread, filling provided 'TMVar' with 'Connection' handle
connect :: MonadIO m
  => Text                                               -- ^ OAuth2 access token
  -> TMVar Connection                                   -- ^ Will be filled with SlackServer
  -> m ()
connect tok tv = do
  nc <- liftIO newBroadcastTChanIO
  cc <- liftIO newTChanIO
  atomically $ putTMVar tv $ Connection nc cc
  connectWithRetries tok nc cc

-- | Runs client in a new thread, returning it's identifier along with 'Connection' handle
-- You are responsible to killing the client process. See also: 'withConnection'
connectAsync :: MonadIO m
  => Text                                               -- ^ OAuth2 access token
  -> m (ThreadId, Connection)
connectAsync tok = do
  ts <- liftIO newEmptyTMVarIO
  t <- liftIO $ fork $ connect tok ts
  s <- atomically $ readTMVar ts
  return (t, s)

-- | Runs provided callback in bracket, supplied with 'Connection' handle
-- When callback finishes (normally or otherwise) slack client process will also be killed.
withConnection :: (MonadMask m, MonadIO m)
  => Text                                               -- ^ OAuth2 access token
  -> (Connection -> m a)                                -- ^ Action to be executed with 'Connection' handle
  -> m a
withConnection tok proc =
  bracket
    (liftIO $ connectAsync tok)
    (\(t, _) -> throwTo t Dismissed)
    (\(_, s) -> proc s)

connectWithRetries :: MonadIO m => Text -> TChan Notification -> TChan Command -> m ()
connectWithRetries tok nc cc = liftIO $
  connectUsing tok nc cc `catchAny` \e -> do
    atomically $ writeTChan nc $ Disconnected e
    connectWithRetries tok nc cc

connectUsing :: Text -> TChan Notification -> TChan Command -> IO ()
connectUsing tok nc cc =
  withSlackHandle (SlackConfig $ unpack tok) $ \h -> do
    let s = stateFromSession $ getSession h
    atomically $ writeTChan nc $ Connected s
    withAsync (serveAPI h s) $ \_ ->
      processEvent h s
  where
    processEvent h s = do
      e <- getNextEvent h
      let s' = update e s
      atomically $ writeTChan nc (SlackEvent e s')
      processEvent h s'
    serveAPI h ss = do
      let mids = mempty
      nc' <- atomically (dupTChan nc)
      processCommand nc' mids ss
      where
        processCommand :: TChan Notification -> Map Int (TMVar SlackTimeStamp) -> SlackState -> IO ()
        processCommand nc' mids s =
          atomically (Left <$> readTChan cc <|> Right <$> readTChan nc') >>= \case
            Left (SendMessage cid msg tmid) -> do
              n <- sendMessage h cid msg
              processCommand nc' (insertMap n tmid mids) s
            Left (GetState ts) -> do
              atomically $ putTMVar ts s
              processCommand nc' mids s
            Right (SlackEvent (MessageResponse n mid _) s') ->
              case updateLookupWithKey (\_ _ -> Nothing) n mids of
                (Just tmid, mids') -> do
                  atomically $ putTMVar tmid mid
                  processCommand nc' mids' s'
                (Nothing, mids') ->
                  processCommand nc' mids' s'
            Right (SlackEvent _ s') ->
              processCommand nc' mids s'
            Right _ ->
              processCommand nc' mids s
