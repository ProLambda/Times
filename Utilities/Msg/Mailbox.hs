{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Utilities.Msg.Mailbox where

import qualified Data.Text.Lazy as T
import qualified Data.Text      as TT

import  Utilities.Msg.Internal.Model
import  Control.Monad.IO.Class
import  Database.Persist
import  Database.Persist.Postgresql
import  Web.Scotty                       (ActionM)
import  Data.Time.Clock
import  Control.Monad.Logger

connStr = "dbname=tutorial host=localhost user=tutorial password=tutorial port=5432"

addCount :: MUserId
         -> String
         -> IO T.Text
addCount name name' = 
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
     liftIO $ flip runSqlPersistMPool pool $ do
       printMigration migrateAll
       user <- get name
       case user of
            Nothing -> return "err"
            Just (MUser _ p) -> do
                 update name [MUserPosts =. (+1) p]
                 return $ T.pack $ name' ++ (show p)


sendMsg :: T.Text  -- from
        -> T.Text  -- to
        -> T.Text  -- content
        -> IO Bool
sendMsg from' to' cont = do
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
     liftIO $ flip runSqlPersistMPool pool $ do
       printMigration migrateAll
       time <- liftIO $ getCurrentTime
       from <- getBy $ UniqueUsername from'
       case from of
         Nothing -> return False
         Just (Entity idf _) -> do
           to <- getBy $ UniqueUsername to'
           case to of
             Nothing -> return False
             Just (Entity idt _) -> do
               uid1 <- liftIO $ addCount idf (T.unpack from')
               uid2 <- liftIO $ addCount idt (T.unpack to')
               if uid1 == "err" || uid2 == "err"
                 then return False
                 else do
                   insert $ MPost uid1 uid2 time from' to' [cont]
                   insert $ MPost uid2 uid1 time from' to' [cont]
                   return True



addMsg :: T.Text  -- author
       -> UTCTime
       -> [T.Text]
       -> [T.Text]
addMsg au time cont =
  cont ++ [T.pack $ (T.unpack au) ++ "@" ++ (take 16 $ show time) ++ " said: "]

replyMsg :: T.Text  -- uid_from
         -> T.Text  -- name
         -> T.Text  -- cont
         -> IO Bool
replyMsg uidf from cont' = 
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
     liftIO $ flip runSqlPersistMPool pool $ do
       printMigration migrateAll
       time <- liftIO $ getCurrentTime
       msg1 <- getBy $ UniqueId uidf
       case msg1 of
         Nothing -> return False
         Just (Entity id1 (MPost _ uidt _ from _ cont)) -> do
           msg2 <- getBy $ UniqueId uidt
           case msg2 of
             Nothing -> return False
             Just (Entity id2 (MPost _ uidt _ _ _ _)) -> do
               let cont' = addMsg from time cont
               update id1 [MPostContent =. cont']
               update id2 [MPostContent =. cont']
               return True
