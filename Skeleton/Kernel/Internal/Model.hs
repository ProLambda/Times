{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Skeleton.Kernel.Internal.Model where

import qualified Data.Text.Lazy      as T
import           Data.Time.Clock
import           Database.Persist.TH
import           Database.Persist.Postgresql
import           Skeleton.Kernel.Internal.Type

connStr :: ConnectionString
connStr = "dbname= host= user= password= port="

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
   User
        email    T.Text
        username T.Text  -- primary key
        password T.Text
        stars    Int     -- number of stars collected
        level    Int
        subscr   Bool
        UniqueUsername  username
        UniqueEmail     email
        deriving Show
   NewsPost
        time      UTCTime
        title     T.Text
        newsUrl   T.Text
        pageId    T.Text  -- link to disqus page
        authorId  UserId
        UniqueUrl pageId
        deriving  Show
        deriving  Eq
   FlagPost
        time     UTCTime
        title    T.Text
        newsUrl  T.Text
        pageId   T.Text   -- link to disqus page
        userId   UserId
        PairFlag pageId userId
        deriving Show
        deriving Eq
   StarPost
        pageId   T.Text
        userId   UserId
        PairStar pageId userId
        deriving Show
        deriving Eq
   Statistics
        totalw      Int
        totala      Int
        totalb      Int
        name        T.Text
        UniqueName  name
        deriving    Show
   World
        author     T.Text
        disqusid   T.Text
        title      T.Text
        url        T.Text
        host       T.Text
        img        T.Text
        intro      T.Text
        time       UTCTime
        week       Week
        stars      Int     -- number of stars collected
        UniqueIdw  disqusid
        deriving   Show
        deriving   Eq
   Alpha
        author     T.Text
        disqusid   T.Text
        title      T.Text
        url        T.Text
        host       T.Text
        img        T.Text
        intro      T.Text
        time       UTCTime
        week       Week
        stars      Int     -- number of stars collected
        UniqueIda  disqusid
        deriving   Show
        deriving   Eq
   Beta
        author     T.Text
        disqusid   T.Text
        title      T.Text
        url        T.Text
        host       T.Text
        img        T.Text
        intro      T.Text
        time       UTCTime
        week       Week
        stars      Int    
        UniqueIdb  disqusid
        deriving   Show
        deriving   Eq
   Country
        cn          Int
        us          Int
        jp          Int
        gb          Int   -- uk
        nl          Int   -- netherlands
        tw          Int   -- taiwan
        hk          Int   -- hong kong
        mo          Int   -- macao
        au          Int   -- australia
        nz          Int   -- new zealand
        ca          Int   -- canada
        name        T.Text
        UniqueId    name
        deriving    Show
        deriving    Eq
  |]
