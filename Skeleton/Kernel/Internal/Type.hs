module Skeleton.Kernel.Internal.Type where

import qualified Data.Text.Lazy      as T
import           Data.Time.Clock

type Week = (T.Text, T.Text)

data RetStatus = Illegal | Pass

data TypePost = News | Asks | Academ
  
type Ele = (String, String, String, String, String, String, String,
            UTCTime, (String, String), Int)
           
type ListOfPost = [((String, String), Ele)]

type Ele' = (UTCTime, String, String, String)

type ListOfPost' = [((String, String), Ele')]

type Ele'' = (String, String, Int, Int)

type ListOfPost'' = [((String, String), Ele'')]

type EleS = (String, String, String, String, UTCTime, Int)

type ListOfPostS = [(String, EleS)]

type EleM = (String, String, String, Int)

type ListOfPostM = [EleM]

type DataEle   = (T.Text, T.Text, T.Text, T.Text, T.Text, T.Text,
                  T.Text, UTCTime, (T.Text, T.Text), Int)
               
type PageCache = T.Text

type CacheList = [DataEle]
