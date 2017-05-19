{-# LANGUAGE OverloadedStrings          #-}
module Skeleton.Kernel.Core.Helper (
         getValueAd'
       , getValueN'
       , getValueF'
       , getValue'
       , getValueW'
       , getValueA'
       , getValueB'
       , getValueS'
       , runReq
       , urlCheck
       , urlWrapper
       , imgCheck
       , getTimeDiff'
       , getTimeDiff
       , months
       , week
       , lunarYear
       , daysMon
       , daysYear
       , dateToWeek
       , dateToWeek'
       , getTime
       , getStar
       ) where

import  qualified Data.Text.Lazy as TL

import  Control.Exception                     (try)
import  Skeleton.Kernel.Internal.Model
import  Skeleton.Kernel.Internal.Type
import  Data.Time.Clock
import  Database.Persist
import  Network.HTTP.Client                   (host, parseRequest, HttpException)


getTimeDiff' :: Int  
             -> Int
             -> Int
             -> Int
             -> Int
             -> Int
             -> Int
             -> Int
             -> Int
             -> Int
             -> Int 
getTimeDiff' y m d h mi y' m' d' h' mi' =
             if (y' < y || m' < m || d' < (d - 15))
                then 1
                else if (d' < d)
                        then 2
                        else if (h' < h)
                             then 3
                             else if (mi' < (mi - 1))
                                     then 4
                                     else 5
                                             

getTimeDiff :: UTCTime
            -> UTCTime
            -> String
getTimeDiff c t = let datec = (take 16 $ show c)
                      datet = (take 16 $ show t)
                      year  = ((read $ take 4 datec) :: Int)
                      mon   = ((read $ take 2 $ drop 5 datec) :: Int)
                      day   = ((read $ take 2 $ drop 8 datec) :: Int)
                      hour  = ((read $ take 2 $ drop 11 datec) :: Int)
                      mins  = ((read $ take 2 $ drop 14 datec) :: Int)
                      year' = ((read $ take 4 datet) :: Int)
                      mon'  = ((read $ take 2 $ drop 5 datet) :: Int)
                      day'  = ((read $ take 2 $ drop 8 datet) :: Int)
                      hour' = ((read $ take 2 $ drop 11 datet) :: Int)
                      min'  = ((read $ take 2 $ drop 14 datet) :: Int) in
                  case (getTimeDiff' year mon day hour mins year' mon' day' hour' min' ) of
                    1 -> " long long ago"
                    2 -> (show (day - day')) ++
                         " day" ++ ((\x -> if x == 1 then "" else "s") (day - day')) ++" ago"
                    3 -> (show (hour - hour')) ++
                         " hour" ++ ((\x -> if x == 1 then "" else "s") (hour - hour')) ++" ago"
                    4 -> (show (mins - min')) ++ " minutes ago"
                    _ -> " just now"


months :: [Int]
months = [31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

week   :: [(String, String)]
week   = [("Sun ", "日曜日 "), ("Mon ", "月曜日 "), ("Tue ", "火曜日 "), ("Wed ", "水曜日 "),
          ("Thu ", "木曜日 "), ("Fri ", "金曜日 "), ("Sat ", "土曜日 ")]

lunarYear :: Int
          -> Bool
lunarYear y = if (y `mod` 100 == 0 && y `mod` 400 == 0) || (y `mod` 100 /= 0 && y `mod` 4 == 0)
                 then True
                 else False

daysYear :: Int
         -> Int
daysYear y | y - 2016 > 0 = if lunarYear (y - 1)
                               then 366 + daysYear (y - 1)
                               else 365 + daysYear (y - 1)
           | otherwise = 0

daysMon  :: Int
         -> Int
         -> Int
daysMon _ 0 = 0
daysMon y m | m == 2 = (if lunarYear y then 29 else 28) + (daysMon y (m - 1))
            | otherwise = (months !! (m - 1)) + (daysMon y (m - 1))

dateToWeek' :: Int
            -> Int
            -> Int
            -> (String, String)
dateToWeek' y m d  = let days = (daysYear y) + (daysMon y (m - 1)) + d + 4 in -- 2016 - 1 - 1 - Fri (+4)
                     week !! (days `mod` 7)

dateToWeek :: UTCTime
           -> (String, String)
dateToWeek utc = let utcs  = take 10 $ show utc
                     week' = dateToWeek' ((read $ take 4 utcs) :: Int)
                                         ((read $ take 2 $ drop 5 utcs) :: Int)
                                         ((read $ take 2 $ drop 8 utcs) :: Int) in
                 ((fst week') ++ utcs ++ " UTC", (snd week') ++ utcs ++ " UTC")


getValueAd' :: (Entity User)
            -> Ele''
getValueAd' (Entity _ (User a b _ c d _)) = (TL.unpack a,
                                             TL.unpack b,
                                             c, d)


getValueN' :: (Entity NewsPost)
           -> Ele'
getValueN' (Entity _ (NewsPost a b c d _ )) = (a,
                                               TL.unpack b,
                                               TL.unpack c,
                                               TL.unpack d)

getValueF' :: (Entity FlagPost)
           -> Ele'
getValueF' (Entity _ (FlagPost a b c d _)) = (a,
                                              TL.unpack b,
                                              TL.unpack c,
                                              TL.unpack d)
                                             
                                             

getValue' :: DataEle
          -> Ele
getValue' (a, b, c, d, e, f, g, h, (w1, w2), i) = (TL.unpack a,
                                                   TL.unpack b,
                                                   TL.unpack c,
                                                   TL.unpack d,
                                                   TL.unpack e,
                                                   TL.unpack f,
                                                   TL.unpack g,
                                                   h, (TL.unpack w1,
                                                       TL.unpack w2),
                                                   i)


getValueW' :: (Entity World)
           -> Ele
getValueW' (Entity _ (World a b c d e f g h (w1, w2) i)) = (TL.unpack a,
                                                            TL.unpack b,
                                                            TL.unpack c,
                                                            TL.unpack d,
                                                            TL.unpack e,
                                                            TL.unpack f,
                                                            TL.unpack g,
                                                            h, (TL.unpack w1,
                                                                TL.unpack w2),
                                                            i)

getValueA' :: (Entity Alpha)
           -> Ele
getValueA' (Entity _ (Alpha a b c d e f g h (w1, w2) i)) = (TL.unpack a,
                                                            TL.unpack b,
                                                            TL.unpack c,
                                                            TL.unpack d,
                                                            TL.unpack e,
                                                            TL.unpack f,
                                                            TL.unpack g,
                                                            h, (TL.unpack w1,
                                                                TL.unpack w2),
                                                            i)

getValueB' :: (Entity Beta)
           -> Ele
getValueB' (Entity _ (Beta a b c d e f g h (w1, w2) i)) = (TL.unpack a,
                                                           TL.unpack b,
                                                           TL.unpack c,
                                                           TL.unpack d,
                                                           TL.unpack e,
                                                           TL.unpack f,
                                                           TL.unpack g,
                                                           h, (TL.unpack w1,
                                                               TL.unpack w2),
                                                           i)


getValueS' :: Ele
           -> EleS
          -- disqusid title url time stars
getValueS' (a, b, c, d, _, _, _, f, _, g) = (a, b, c, d, f, g)


getTime :: Ele
        -> UTCTime
        
getTime (_, _, _, _, _, _, _, t, _, _) = t


getStar :: Ele
        -> Int
getStar (_, _, _, _, _, _, _, _, _, t) = t


----------------------------------------------------------------------
-- Analyser
----------------------------------------------------------------------

suffix :: [String]
suffix = [".jpg", ".png", ".gif"]


defaultImg :: String
defaultImg = "./img/lambda-default.png"


imgCheck :: String -> String
imgCheck "" = defaultImg
imgCheck x  | len <= 4 = defaultImg
            | otherwise = if drop (len - 4) x `elem` suffix
                             then x
                             else defaultImg
              where
                len = length x


runReq :: String
        -> IO (String, String)
runReq url = if url == "/#"
                then return ("/#" , "Ask")
                else do
                     parse <- try $ parseRequest url
                     case parse of
                          Left  e -> do
                                     print (e :: HttpException)
                                     return ("err", "err")
                          Right p -> return (url, tail . init . show $ host p)


urlWrapper :: String -> String
urlWrapper x | x == "/#" = x
             | otherwise = if take 4 x == "http"
                              then x
                              else "err"

urlCheck :: String -> Bool
urlCheck x  | x == "/#"    = True
            | length x > 4 = True
            | otherwise    = False
