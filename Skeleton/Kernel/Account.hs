{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Skeleton.Kernel.Account (
         authUser
       , addUser
       , resetPass
       , getMailAddr
       , deletePost
       , deleteFlag
       , getStars
       , getFlag
       , getNews
       , getUser
       , checkNoUser
       , checkNoStar
       , checkNoFlag
       , updateStarPost
       , updateFlagPost
       , updateUserPost
       , updateStarCount
       , getLevel
       , setLevel
       , setSubscr
       , getSubscr
       , getUserById
       ) where

import qualified Data.Text.Lazy                 as T

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Postgresql
import           Prelude                        hiding (id)
import           Skeleton.Kernel.Internal.Model
import           Skeleton.Kernel.PostHelper

authUser :: String
         -> IO (Maybe (Entity User))
authUser user =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      printMigration migrateAll
      selectFirst [UserUsername ==. (T.pack user)] []


-------------------------------------------------------------
-- add new user
-------------------------------------------------------------

checkNoUser :: T.Text
            -> T.Text
            -> IO Int
checkNoUser s m =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      s1 <- selectFirst [UserUsername ==. s] []
      m1 <- selectFirst [UserEmail ==. m] []
      if (length s1) > 0
        then return 1
        else if (length m1) > 0
                then return 2
                else return 0


addUser :: T.Text   -- email
        -> T.Text   -- username
        -> T.Text   -- password
        -> IO ()
addUser mail user pass =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      insert $ User mail user pass 0 8 True
      return ()


resetPass :: T.Text
          -> T.Text
          -> IO ()
resetPass pass name =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername name
      case maybeEle of
        Nothing            -> return ()
        Just (Entity id _) -> update id [UserPassword =. pass]


getUser :: Int
        -> IO [Entity User]
getUser ifadmin =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      selectList [UserLevel <. ifadmin] []



getLevel :: T.Text
         -> IO Int
getLevel name =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername name
      case maybeEle of
        Nothing                            -> return (-1)
        Just (Entity _ (User _ _ _ _ l _)) -> return l


setLevel :: T.Text
         -> Int             -- limit
         -> (Int -> Int)
         -> IO ()
setLevel name limit level =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername name
      case maybeEle of
        Nothing -> return ()
        Just (Entity uid _) -> do
          guser <- get uid
          case guser of
            Nothing -> return ()
            Just (User _ _ _ _ l _) ->
              if level l >= limit then update uid [UserLevel =. limit - 1]
                                  else update uid [UserLevel =. level l]

-------------------------------------------------------------
-- update post record
-------------------------------------------------------------

updateStarCount :: T.Text
                -> IO ()
updateStarCount name =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername name
      case maybeEle of
        Nothing -> return ()
        Just (Entity uid ele) -> do
          update uid [UserStars =. (+1) (getstar ele)]
          return ()
        where
          getstar (User _ _ _ s _ _) = s


updateUserPost :: UTCTime
               -> T.Text
               -> T.Text
               -> T.Text
               -> T.Text
               -> IO ()
updateUserPost time title url page author =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername author
      case maybeEle of
          Nothing -> return ()
          Just (Entity uid _) -> do
               insert $ NewsPost time title url page uid
               return ()


checkNoFlag :: T.Text
            -> T.Text
            -> IO Bool
checkNoFlag pid u =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername u
      case maybeEle of
        Nothing -> return False
        Just (Entity uid _) -> do
          s <- selectList [FlagPostPageId ==. pid, FlagPostUserId ==. uid] []
          return (if (length s) > 0 then False else True)


updateFlagPost :: UTCTime
               -> T.Text
               -> T.Text
               -> T.Text
               -> T.Text
               -> IO ()
updateFlagPost time title url page author =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername author
      case maybeEle of
          Nothing -> return ()
          Just (Entity uid _) -> do
               insert $ FlagPost time title url page uid
               return ()


checkNoStar :: T.Text
            -> T.Text
            -> IO Bool
checkNoStar pid u =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername u
      case maybeEle of
        Nothing -> return False
        Just (Entity uid _ ) -> do
          s <- selectList [StarPostPageId ==. pid, StarPostUserId ==. uid] []
          return (if (length s) > 0 then False else True)


updateStarPost :: T.Text
               -> T.Text
               -> IO ()
updateStarPost pid author =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername author
      case maybeEle of
          Nothing -> return ()
          Just (Entity uid _) -> do
               insert $ StarPost pid uid
               return ()


getNews :: T.Text
        -> IO [Entity NewsPost]
getNews name =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername name
      case maybeEle of
        Nothing -> return []
        Just (Entity uid _) ->
             selectList [NewsPostAuthorId ==. uid] []


getFlag :: T.Text
        -> IO [Entity FlagPost]
getFlag name =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername name
      case maybeEle of
        Nothing -> return []
        Just (Entity uid _) ->
             selectList [FlagPostUserId ==. uid] []


getStars :: T.Text
         -> IO Int
getStars name =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      maybeEle <- getBy $ UniqueUsername name
      case maybeEle of
        Nothing                            -> return 0
        Just (Entity _ (User _ _ _ s _ _)) -> return s

-------------------------------------------------------------
-- add new user
-------------------------------------------------------------


deleteFlag :: T.Text    -- name
           -> T.Text    -- pid
           -> IO ()
deleteFlag name pid =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      user <- getBy $ UniqueUsername name
      case user of
        Nothing -> return ()
        Just (Entity uid _) -> do
             deleteWhere [FlagPostPageId ==. pid, FlagPostUserId ==. uid]
             return ()


deletePost :: T.Text    -- name
           -> T.Text    -- pid
           -> Int       -- line
           -> IO ()
deletePost name pid line =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      user <- getBy $ UniqueUsername name
      case user of
        Nothing -> return ()
        Just (Entity uid _) -> do
             deleteWhere [FlagPostPageId ==. pid]
             deleteWhere [StarPostPageId ==. pid]
             deleteWhere [NewsPostPageId ==. pid, NewsPostAuthorId ==. uid]
             liftIO $ deletePost' pid line
             return ()


getMailAddr :: IO [String]
getMailAddr =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      xs <- selectList [UserLevel >=. 8, UserSubscr ==. True] []
      return $ userAddr xs
      where
        userAddr' (Entity _ (User a _ _ _ _ _)) = T.unpack a
        userAddr  [] = []
        userAddr  xs = map userAddr' xs


setSubscr :: T.Text
          -> IO ()
setSubscr user =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      x <- selectFirst [UserUsername ==. user] []
      case x of
        Nothing -> return ()
        Just (Entity uid (User _ _ _ _ _ s)) ->
          update uid [UserSubscr =. not s]


getSubscr :: T.Text
          -> IO Bool
getSubscr user =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      x <- selectFirst [UserUsername ==. user] []
      case x of
        Nothing                            -> return False
        Just (Entity _ (User _ _ _ _ _ s)) -> return s


getUserById :: T.Text
            -> IO T.Text
getUserById uid =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      x <- selectFirst [NewsPostPageId ==. uid] []
      case x of
        Nothing -> return "guest"
        Just (Entity _ (NewsPost _ _ _ _ a)) -> do
             user <- get a
             case user of
               Nothing                    -> return "guest"
               Just (User _ name _ _ _ _) -> return name
