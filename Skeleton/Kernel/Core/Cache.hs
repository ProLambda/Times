{-# LANGUAGE OverloadedStrings          #-}
module Skeleton.Kernel.Core.Cache (
         helperDelete
       , helperStar
       , initCache
       , renderCacheNow'
       , renderCache'
       , inCache
       , notInCache
       , CacheList
       , PageCache
       , insertCache
       )where

import  qualified Data.Text.Lazy as T

import  Control.Monad                       (forM_) 
import  Control.Concurrent                  (MVar, newMVar, modifyMVar_, readMVar, threadDelay)
import  Prelude                             hiding (id)
import  Skeleton.Kernel.Internal.Type
import  Skeleton.Kernel.Core.Sort


pageCache :: PageCache
pageCache = ""

newsCache :: CacheList
newsCache = []

poolSize :: Int
poolSize = 500


addEle :: DataEle
       -> CacheList
       -> CacheList
addEle a []     = [a]
addEle a (x:xs) | length (x:xs) < poolSize = a : x : xs
                | otherwise                = a : x : (init xs)


getId :: DataEle
      -> T.Text
getId (_, x, _, _, _, _, _, _, _, _) = x


update' :: DataEle
        -> DataEle
update' (a, b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, f, g, h, i, j + 1)


notInCache :: T.Text
        -> MVar CacheList
        -> IO Bool
notInCache id cache = do
  xs <- readMVar cache
  let ans = filter (\x -> (==) (getId x) id) xs
  return $ null ans


inCache :: T.Text
        -> MVar CacheList
        -> IO Bool
inCache id cache = do
  xs <- readMVar cache
  let ans = filter (\x -> (==) (getId x) id) xs
  return . not $ null ans


deleteById :: T.Text
           -> CacheList
           -> CacheList
deleteById id xs = filter (\x -> (/=) (getId x) id) xs


updateById :: T.Text
           -> CacheList
           -> CacheList
updateById id xs = map (\x -> if getId x == id
                                 then update' x
                                 else x)
                       xs


initCache' :: MVar CacheList
           -> CacheList
           -> IO (MVar CacheList)
initCache' cache filts = do
  forM_ filts $ \y -> 
    modifyMVar_ cache $ \x -> return $ addEle y x
  modifyMVar_ cache $ \x -> return $ sortL x
  return cache

insertCache :: DataEle
            -> MVar CacheList
            -> IO ()
insertCache x xs = do
  modifyMVar_ xs $ \y -> return $ addEle x y

deleteNews :: T.Text
           -> MVar CacheList
           -> IO ()
deleteNews id xs =
  modifyMVar_ xs $ \x -> return $ deleteById id x

updateStar :: T.Text
           -> MVar CacheList
           -> IO ()
updateStar id xs =
  modifyMVar_ xs $ \x -> return $ updateById id x


helperDelete :: String
             -> Int
             -> (MVar CacheList, MVar CacheList, MVar CacheList)
             -> IO ()
helperDelete id' op (cw, cb, ca) = do
  let id = T.pack id'
  case op of
    1 -> deleteNews id cw
    2 -> deleteNews id ca
    _ -> deleteNews id cb

helperStar :: String
           -> Int
           -> (MVar CacheList, MVar CacheList, MVar CacheList)
           -> IO ()
helperStar id' op (cw, cb, ca) = do
  let id = T.pack id'
  case op of
    1 -> updateStar id cw
    2 -> updateStar id ca
    _ -> updateStar id cb

initCache :: (CacheList, CacheList, CacheList)
          -> IO (MVar CacheList, MVar CacheList, MVar CacheList,
                 MVar PageCache, MVar PageCache, MVar PageCache)
initCache (world, alpha, beta) = do
  pagew   <- newMVar pageCache
  pagea   <- newMVar pageCache
  pageb   <- newMVar pageCache
  cachew' <- newMVar newsCache
  cacheb' <- newMVar newsCache
  cachea' <- newMVar newsCache
  cachew  <- initCache' cachew' world
  cachea  <- initCache' cachea' alpha
  cacheb  <- initCache' cacheb' beta
  return (cachew, cacheb, cachea,
          pagew , pageb , pagea)


renderCacheNow' :: MVar PageCache
                -> T.Text
                -> IO ()
renderCacheNow' page web = do
  modifyMVar_ page $ \_ -> return web


renderCache' :: [MVar PageCache]
             -> [IO T.Text]
             -> IO ()
renderCache' pages webs= do
  forM_ (zip webs pages) $ \(x', b) -> do
    x <- x'
    modifyMVar_ b $ \_ -> return x
  threadDelay $ 90 * 1000000           -- 1.5 minutes
