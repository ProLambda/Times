--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Utilities.Chat.Server (
       chatroom
       ) where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever, when)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Data.Time.Clock
import qualified Data.Text as T

import qualified Network.WebSockets as WS


--------------------------------------------------------------------------------
type Client = (Text, WS.Connection)

type ServerState = [Client]

type MsgCache = [Text]

newServerState :: ServerState
newServerState = []

newMsgCache :: MsgCache
newMsgCache = []

clientExists :: Client
             -> ServerState
             -> Bool
clientExists client = any ((== fst client) . fst)


addClient :: Client
          -> ServerState
          -> ServerState
addClient client clients = client : clients


addMsg :: Text
       -> MsgCache
       -> MsgCache
addMsg msg [] = [msg]
addMsg msg (x:xs) | length (x:xs) < 50 = (x:xs) ++ [msg]
                  | otherwise          = xs     ++ [msg]


removeClient :: Client
             -> ServerState
             -> ServerState
removeClient client = filter ((/= fst client) . fst)


broadcast :: Text
          -> ServerState
          -> IO ()
broadcast message clients = do
  -- T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

chatroom :: IO ()
chatroom = do
  state <- newMVar newServerState
  msgcache <- newMVar newMsgCache
  WS.runServer "0.0.0.0" 9000 $ application msgcache state


application :: MVar MsgCache
            -> MVar ServerState
            -> WS.ServerApp
application mcache' state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30  -- ensure the connection stays alive
  msg <- WS.receiveData conn
  clients <- readMVar state
  case msg of
    _ | clientExists client clients ->
              WS.sendTextData conn ("User already exists" :: Text)
      | otherwise -> flip finally disconnect $ do
            modifyMVar_ state $ \s -> do
              let s' = addClient client s
              WS.sendTextData conn $
                "Welcome! Users: " `mappend`
                T.intercalate ", " (map fst s)
              msg' <- readMVar mcache'
              when (msg' /= []) $ WS.sendTextDatas conn msg'
              broadcast (fst client `mappend` " joined") s'
              return s'
            talk conn mcache' state client
      where
        client     = (msg, conn)
        disconnect = do
          s <- modifyMVar state $ \s ->
            let s' = removeClient client s in return (s', s')
          broadcast (fst client `mappend` " disconnected") s


talk :: WS.Connection
     -> MVar MsgCache
     -> MVar ServerState
     -> Client
     -> IO ()
talk conn mcache' state (user, _) = forever $ do
  msg <- WS.receiveData conn
  ctime <- liftIO $ getCurrentTime
  let time = T.pack $ drop 11 $ take 16 $ show ctime
      msg' = user `mappend` "@" `mappend` time `mappend` ": " `mappend` msg
  modifyMVar_ mcache' $ \x -> return $ addMsg msg' x
  readMVar state >>= broadcast msg'
    
