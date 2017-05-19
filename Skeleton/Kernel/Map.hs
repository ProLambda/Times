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

module Skeleton.Kernel.Map (
         initializeStatDb
       , modifyStat
       , buildMap
       ) where

import  Skeleton.Kernel.Internal.Model

import  Control.Monad.Logger
import  Control.Monad.IO.Class
import  Database.Persist
import  Database.Persist.Postgresql
import  Web.Scotty                       (ActionM)

-- must be called before scotty
initializeStatDb :: IO ()
initializeStatDb = 
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      _ <- insert $ Country 0 0 0 0 0 0 0 0 0 0 0 "country"
      return ()

cn :: Country -> Int
cn (Country x _ _ _ _ _ _ _ _ _ _ _) = x

us :: Country -> Int
us (Country _ x _ _ _ _ _ _ _ _ _ _) = x

jp :: Country -> Int
jp (Country _ _ x _ _ _ _ _ _ _ _ _) = x

gb :: Country -> Int
gb (Country _ _ _ x _ _ _ _ _ _ _ _) = x

nl :: Country -> Int
nl (Country _ _ _ _ x _ _ _ _ _ _ _) = x

tw :: Country -> Int
tw (Country _ _ _ _ _ x _ _ _ _ _ _) = x

hk :: Country -> Int
hk (Country _ _ _ _ _ _ x _ _ _ _ _) = x

mo :: Country -> Int
mo (Country _ _ _ _ _ _ _ x _ _ _ _) = x

au :: Country -> Int
au (Country _ _ _ _ _ _ _ _ x _ _ _) = x

nz :: Country -> Int
nz (Country _ _ _ _ _ _ _ _ _ x _ _) = x

ca :: Country -> Int
ca (Country _ _ _ _ _ _ _ _ _ _ x _) = x

-------------------------------------------------------------
-- modify stat
-------------------------------------------------------------

modifyStat :: String
           -> ActionM ()
modifyStat cou =
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll

      maybeEle <- getBy $ UniqueId "country"
      case maybeEle of
           Nothing -> return ()
           Just (Entity uid stat) ->
             case cou of
               "CN" -> update uid [CountryCn =. (+1) (cn stat)]
               "US" -> update uid [CountryUs =. (+1) (us stat)]
               "JP" -> update uid [CountryJp =. (+1) (jp stat)]
               "GB" -> update uid [CountryGb =. (+1) (gb stat)]
               "NL" -> update uid [CountryNl =. (+1) (nl stat)]
               "TW" -> update uid [CountryTw =. (+1) (tw stat)]
               "HK" -> update uid [CountryHk =. (+1) (hk stat)]
               "MO" -> update uid [CountryMo =. (+1) (mo stat)]
               "AU" -> update uid [CountryAu =. (+1) (au stat)]
               "NZ" -> update uid [CountryNz =. (+1) (nz stat)]
               "CA" -> update uid [CountryCa =. (+1) (ca stat)]           
               _ -> return ()
             

w :: (Country -> Int)
  -> Country
  -> String  -- wrapper
w f x = "\"" ++ (show $ f x) ++ "\""

jsw :: String
    -> String
jsw x = "var sample_data = {" ++ x ++ "};"

buildMap :: IO String
buildMap = do
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll

      maybeEle <- getBy $ UniqueId "country"
      case maybeEle of
        Nothing -> return $ jsw ""
        Just (Entity _ stat) -> do
          let cn' = ",\"cn\":" ++ (w cn stat)
              us' = ",\"us\":" ++ (w us stat) ++ cn'
              jp' = ",\"jp\":" ++ (w jp stat) ++ us'
              gb' = ",\"gb\":" ++ (w gb stat) ++ jp'
              nl' = ",\"nl\":" ++ (w nl stat) ++ gb'
              tw' = ",\"tw\":" ++ (w tw stat) ++ nl'
              hk' = ",\"hk\":" ++ (w hk stat) ++ tw'
              mo' = ",\"mo\":" ++ (w mo stat) ++ hk'
              au' = ",\"au\":" ++ (w au stat) ++ mo'
              nz' = ",\"nz\":" ++ (w nz stat) ++ au'
              ca' = "\"ca\":"  ++ (w ca stat) ++ nz'
          return $ jsw ca'
        
