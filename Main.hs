{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad
-- import  Control.Monad.IO.Class
import           Control.Concurrent                   (MVar, forkIO,
                                                       threadDelay)
import           Database.Persist
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           Skeleton.Home
import           Skeleton.Kernel.Core.Cache
import           Skeleton.Kernel.Core.Mail
import           Skeleton.Kernel.Internal.Model
import           Skeleton.Kernel.Post
import           System.Environment                   (getEnv)
import           Utilities.Chat.Server
import           Utilities.Login.Session
import           Web.Scotty

import           Skeleton.Kernel.Internal.Type
import           Skeleton.Shell.Template

import           Data.List                            (isSuffixOf)
import           System.Directory
import           System.IO


-- import Utilities.Msg.Mailbox
-- import  Skeleton.Kernel.Map
-- import  Skeleton.Kernel.Account

-- prepare session configuration
config :: SessionConfig
config = defaultSessionConfig

worldInit :: IO CacheList
worldInit = do
  news <- getWorldNews
  return $ map filt news
  where
    filt (Entity _ (World a b c d e f g h i j)) = (a, b, c, d, e, f, g, h, i, j)

alphaInit :: IO CacheList
alphaInit = do
  news <- getAlphaNews
  return $ map filt news
  where
    filt (Entity _ (Alpha a b c d e f g h i j)) = (a, b, c, d, e, f, g, h, i, j)

betaInit :: IO CacheList
betaInit = do
  news <- getBetaNews
  return $ map filt news
  where
    filt (Entity _ (Beta a b c d e f g h i j)) = (a, b, c, d, e, f, g, h, i, j)

loop :: (MVar CacheList, MVar CacheList, MVar CacheList)
     -> IO ()
loop cache = do
  mailLoop cache
  threadDelay $ 7 * 86400 * 1000000  -- once a week

loopHeaderImg :: String -> String -> IO ()
loopHeaderImg src dest = do
  fileNames' <- listDirectory src
  let fileNames = filter (\x -> isSuffixOf ".jpg" x) fileNames'
  forM_ fileNames $ \headerimg -> do
    -- rename old header to temp
    print ("change background img to " ++ headerimg)
    renameFile (dest ++ "header.jpg") (src ++ "temp.jpg")
    renameFile (src ++ headerimg) (dest ++ "header.jpg")
    renameFile (src ++ "temp.jpg") (src ++ headerimg)
    threadDelay $ 10000000


routes :: (MVar CacheList, MVar CacheList, MVar CacheList,
           MVar PageCache, MVar PageCache, MVar PageCache)
       -> ScottyM ()
routes (cw, cb, ca, pw, pb, pa) = do
    -- WAI middleware are run and mateched top-down
    -- Serving static content, including images, html, css and javascripts
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware $ staticPolicy (noDots >-> addBase "imgs")

    -- log status
    middleware logStdoutDev

    -- routes are run and mateched top-down
    home
    >> (login      config)
    >> logout
    >> (world      pw)
    >> (alpha      pa)
    >> (beta       pb)
    >> (union      (pw, pb, pa))
    >> disqus
    >> chat
    >> (submitnews (cw, cb, ca) (pw, pb, pa))
    >> (deletepost (cw, cb, ca) (pw, pb, pa))
    >> about
    >> search
    >> setting
    >> dashboard
    >> (purify     (cw, cb, ca) (pw, pb, pa))
    >> signup
    >> (starpost   (cw, cb, ca) (pw, pb, pa))
    >> flagpost
    >> dashOp
    >> uploadimg
    >> notfound

main :: IO ()
main = do
  -- initializeStatDb
  -- initializeNewsDb
  -- setLevel "xxx" 12 (+1)

  -- cache begin
  w    <- worldInit
  a    <- alphaInit
  b    <- betaInit
  port <- liftM read $ getEnv "PORT"
  (cw, cb, ca, pw, pb, pa) <- initCache (w, a, b)
  _ <- forkIO . forever $ renderCache [cw, cb, ca]
                                      [News, Academ, Asks]
                                      [pw, pb, pa]
  -- cache end

  _ <- forkIO chatroom                       -- chatroom
  _ <- forkIO . forever $ loop (cw, cb, ca)  -- send mails once a week
  -- change header img once a day
  _ <- forkIO . forever $ loopHeaderImg "./static/headerimg/" "./static/dist/img/"
  -- reload the session database into memory, must be called before scotty
  initializeCookieDb config
  scotty port $ routes (cw, cb, ca, pw, pb, pa)

