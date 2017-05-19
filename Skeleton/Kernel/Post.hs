{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Skeleton.Kernel.Post (
         initializeNewsDb
       , modifyNews
       , updateNewsCount
       , checkNoNews
       , insertNews
       , getOneNews
       , getWorldNews
       , getAlphaNews
       , getBetaNews
       ) where

import  qualified Data.Text.Lazy          as TL

import  Skeleton.Kernel.Internal.Model
import  Skeleton.Kernel.Internal.Type
import  Skeleton.Kernel.Account         
import  Control.Monad.IO.Class
import  Data.Time.Clock
import  Database.Persist
import  Database.Persist.Postgresql
import  Control.Monad.Logger
import  Web.Scotty                        (ActionM)
import  Skeleton.Kernel.Core.Helper       hiding (week)

import  Skeleton.Kernel.Core.Cache
import  Control.Concurrent                (MVar)

-- must be called before scotty
initializeNewsDb :: IO ()
initializeNewsDb = 
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      ida <- insert $ Statistics 0 0 0 "unique"
      out <- get ida
      liftIO $ print (out)
      return ()

-------------------------------------------------------------
-- modify news
-------------------------------------------------------------

modifyNews :: Int
           -> TL.Text
           -> IO ()
modifyNews op uniqueId =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      case op of
        1 -> do
          maybeEle <- getBy $ UniqueIdw uniqueId
          case maybeEle of
            Nothing -> return ()
            Just (Entity uid news) -> 
              update uid [WorldStars =. (+1) (getStarsW news)]
        2 -> do
          maybeEle <- getBy $ UniqueIda uniqueId
          case maybeEle of
            Nothing -> return ()
            Just (Entity uid news) -> 
              update uid [AlphaStars =. (+1) (getStarsA news)]
        3 -> do
          maybeEle <- getBy $ UniqueIdb uniqueId
          case maybeEle of
            Nothing -> return ()
            Just (Entity uid news) -> 
              update uid [BetaStars =. (+1) (getStarsB news)]
        _ -> liftIO $ print "impossible error!"
        where
          getStarsW (World _ _ _ _ _ _ _ _ _ s) = s
          getStarsA (Alpha _ _ _ _ _ _ _ _ _ s) = s
          getStarsB (Beta  _ _ _ _ _ _ _ _ _ s) = s

-------------------------------------------------------------
-- insert one news
-------------------------------------------------------------

updateNewsCount :: Int
                -> IO String
updateNewsCount line = do
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueName "unique"
      case maybeEle of
        Nothing -> return "impossible"
        Just (Entity uid ele) -> 
          case line of
            1 -> do
              update uid [StatisticsTotalw =. (+1) (getTotW ele)]
              return (show $ getTotW ele)
            2 -> do
              update uid [StatisticsTotala =. (+1) (getTotA ele)]
              return (show $ getTotA ele)
            _ -> do
              update uid [StatisticsTotalb =. (+1) (getTotB ele)]
              return (show $ getTotB ele)
          where
            getTotW (Statistics s _ _ _ ) = s 
            getTotA (Statistics _ s _ _ ) = s
            getTotB (Statistics _ _ s _ ) = s


checkNoNews :: TL.Text
            -> Int
            -> IO Bool
checkNoNews u radio = 
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      case radio of
        1 -> do
          s <- selectFirst [WorldUrl ==. u] []
          return (if (length s) > 0 then False else True)
        2 -> do
          s <- selectFirst [AlphaUrl ==. u] []
          return (if (length s) > 0 then False else True)
        3 -> do
          s <- selectFirst [BetaUrl ==. u] []
          return (if (length s) > 0 then False else True)
        _ -> return False


insertNews :: Int
           -> TL.Text
           -> TL.Text
           -> TL.Text
           -> TL.Text
           -> TL.Text
           -> TL.Text
           -> String
           -> (MVar CacheList, MVar CacheList, MVar CacheList)
           -> ActionM ()
insertNews op author title url host imgO intro idV (cw, cb, ca) =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      ct <- liftIO $ getCurrentTime
      let img = TL.pack $ imgCheck $ TL.unpack imgO
          pageid = case op of
                        1 -> (TL.pack $ (++) "worldnewid" idV)
                        2 -> (TL.pack $ (++) "alphanewid" idV)
                        _ -> (TL.pack $ (++) "betanewid" idV)
          (w1, w2) = dateToWeek ct
          week = (TL.pack w1, TL.pack w2)
      case op of
        1 -> do
          liftIO $ insertCache (author, pageid, title, url, host, img, intro, ct, week, 0) cw
          insert $ World author pageid title url host img intro ct week 0
          return ()
        2 -> do
          liftIO $ insertCache (author, pageid, title, url, host, img, intro, ct, week, 0) ca
          insert $ Alpha author pageid title url host img intro ct week 0 
          return ()
        _ -> do
          liftIO $ insertCache (author, pageid, title, url, host, img, intro, ct, week, 0) cb
          insert $ Beta author pageid title url host img intro ct week 0 
          return ()
      liftIO $ updateUserPost ct title url pageid author
      return ()
 

-------------------------------------------------------------
-- get news
-------------------------------------------------------------
    
getWorldNews :: IO [Entity World]
getWorldNews =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      -- choose which stars >= 0, for those which stars < 0 are forbiddened
      selectList [WorldStars >=. 0] []  

getAlphaNews :: IO [Entity Alpha]
getAlphaNews =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      selectList [AlphaStars >=. 0] []

getBetaNews :: IO [Entity Beta]
getBetaNews =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      selectList [BetaStars >=. 0] []

defaultEle :: UTCTime
          -> Ele
defaultEle u = ("", "impossibleid0", "Illegal Page", "", "", "",
                "The possible reason for you viewing this page is\
                \ that you pass the WRONG url, fix it for accessing\
                \ the page you really want", u, ("unknown", "unknown"), 0) 


getOneNews :: Int
           -> String
           -> IO Ele
getOneNews num pid =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      time <- liftIO $ getCurrentTime
      case num of
        1 -> do
          xs <- selectList [WorldDisqusid ==. TL.pack pid] [LimitTo 1]
          if xs == []
             then return $ defaultEle time
             else return $ getValueW' $ (xs !! 0)
        2 -> do
          xs <- selectList [AlphaDisqusid ==. TL.pack pid] [LimitTo 1]
          if xs == []
             then return $ defaultEle time
             else return $ getValueA' $ (xs !! 0)
        _ -> do
          xs <- selectList [BetaDisqusid ==. TL.pack pid] [LimitTo 1]
          if xs == []
             then return $ defaultEle time
             else return $ getValueB' $ (xs !! 0)

