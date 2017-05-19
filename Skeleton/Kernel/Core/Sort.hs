{-# LANGUAGE OverloadedStrings          #-}
module Skeleton.Kernel.Core.Sort (
         sort
       , sortM
       , sortL
       ) where

import  Data.Time.Clock
import  Data.List                     (sortOn)
import  Skeleton.Kernel.Core.Helper   (getTime, getStar)
import  Skeleton.Kernel.Internal.Type

gravity :: Float
gravity = 2.0

tiltIndex :: Float
tiltIndex = 10

computeWeight' :: UTCTime
               -> UTCTime
               -> Float
computeWeight' u1 u2 =
               (fromIntegral $ fromEnum (diffUTCTime u1 u2) `div` 60000000000000) / 60.0 + 1.0


computeWeight :: UTCTime
              -> Int
              -> UTCTime
              -> Float
computeWeight u1 star u2 =
              (fromIntegral star + tiltIndex) ** gravity / (computeWeight' u1 u2)


compareE :: (Float, Ele)
        -> Float
compareE (f, _) = 0 - f


sort :: UTCTime
     -> [Ele]
     -> [Ele]
sort _ [] = []
sort u xs = snd $ unzip $
                  sortOn compareE $
                         zip (map (\x -> computeWeight u (fst x) (snd x))
                                  (zip (map getStar xs)
                                       (map getTime xs)))
                             xs

compareEM :: EleM  -- mail sort
          -> Int
compareEM (_, _, _, s) = 0 - s


sortM :: [EleM]
      -> [EleM]
sortM [] = []
sortM xs = take 10 $ sortOn compareEM xs


compareEL :: DataEle  -- latest news
          -> UTCTime
compareEL (_, _, _, _, _, _, _, u, _, _) = u


sortL :: [DataEle]
      -> [DataEle]
sortL [] = []
sortL xs = reverse $ sortOn compareEL xs


