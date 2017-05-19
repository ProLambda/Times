{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Skeleton.Kernel.PostHelper (
         getName
       , deletePost'
       ) where

import  qualified Data.Text.Lazy         as TL

import  Skeleton.Kernel.Internal.Model
import  Database.Persist
import  Database.Persist.Postgresql
import  Control.Monad.IO.Class
import  Control.Monad.Logger

getName :: TL.Text
        -> Int
        -> IO TL.Text
getName pid op =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      case op of
        1 -> do
          x:_ <- selectList [WorldDisqusid ==. pid] [LimitTo 1]
          return $ getNameW x
        2 -> do
          x:_ <- selectList [AlphaDisqusid ==. pid] [LimitTo 1]
          return $ getNameA x
        _ -> do
          x:_ <- selectList [BetaDisqusid ==. pid] [LimitTo 1]
          return $ getNameB x
        where
          getNameW (Entity _ (World a _ _ _ _ _ _ _ _ _)) = a
          getNameA (Entity _ (Alpha a _ _ _ _ _ _ _ _ _)) = a
          getNameB (Entity _ (Beta  a _ _ _ _ _ _ _ _ _)) = a


deletePost' :: TL.Text
            -> Int
            -> IO ()
deletePost' pid op =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      case op of
        1 -> deleteWhere [WorldDisqusid ==. pid]
        2 -> deleteWhere [AlphaDisqusid ==. pid]
        _ -> deleteWhere [BetaDisqusid ==. pid]
   
