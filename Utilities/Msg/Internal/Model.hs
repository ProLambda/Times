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

module Utilities.Msg.Internal.Model where

import qualified Data.Text.Lazy      as T
import           Data.Time.Clock
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
   MUser
        username T.Text  -- primary key
        posts    Int
        UniqueUsername  username
        deriving Show
   MPost
        uidf      T.Text
        uidt      T.Text
        time      UTCTime
        from      T.Text
        to        T.Text
        content   [T.Text]
        UniqueId  uidf
        deriving  Show
        deriving  Eq
  |]
