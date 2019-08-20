{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skeleton.Home
       (   home
         , submitnews
         , uploadimg
         , notfound
         , logout
         , login
         , signup
         , world
         , alpha
         , beta
         , disqus
         , about
         , setting
         , dashboard
         , dashOp
         , starpost
         , flagpost
         , deletepost
         , purify
         , chat
         , search
         , union
       ) where

import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy          as B
import qualified Data.Text                     as TS
import qualified Data.Text.Lazy                as T

import           Control.Monad                 (when)
import           Control.Monad.IO.Class
import           Database.Persist              hiding (get)
import           Skeleton.Kernel.Account
import           Skeleton.Kernel.Core.Helper
import           Skeleton.Kernel.Post
import           Skeleton.Shell.Template
import           Utilities.Login.Session
import           Web.Scotty

import           Data.Char
import           Data.Time.Clock
import           Prelude                       hiding (id)
import           Skeleton.Kernel.Map

import           Control.Concurrent            (MVar, forkIO, readMVar)
import           Network.Wai.Parse
import           Skeleton.Kernel.Core.Cache
import           Skeleton.Kernel.Core.Secure
import           Skeleton.Kernel.Internal.Type
import           Web.Scotty.Cookie

import           System.FilePath               ((</>))

-------------------------------------------------------------
-- helper cache -- refresh cache or not
-------------------------------------------------------------

helperRender :: T.Text
             -> Int
             -> (MVar CacheList, MVar CacheList, MVar CacheList)
             -> (MVar PageCache, MVar PageCache, MVar PageCache)
             -> (T.Text -> MVar CacheList -> IO Bool)
             -> IO ()
helperRender id op' (w, b, a) (w', b', a') f = do
  let (op, xs, ys) = case op' of
                       1 -> (News  , w, w')
                       3 -> (Academ, b, b')
                       _ -> (Asks  , a, a')
  doit <- f id xs
  when doit $ renderCacheNow xs op ys

-------------------------------------------------------------
-- routes
-------------------------------------------------------------

-- for ajax serving
-- world, aplha, beta lines
union :: (MVar PageCache, MVar PageCache, MVar PageCache)
      -> ScottyM ()
union (pw, pb, pa) = get "/union" $ do
      w <- liftIO $ readMVar pw
      a <- liftIO $ readMVar pa
      b <- liftIO $ readMVar pb
      json [w, b, a]

world :: MVar PageCache
      -> ScottyM ()
world w = get "/world" $ do
          web <- liftIO $ readMVar w
          html web

alpha :: MVar PageCache
      -> ScottyM ()
alpha a = get "/alpha" $ do
          web <- liftIO $ readMVar a
          html web


beta :: MVar PageCache
     -> ScottyM ()
beta b = get "/beta" $ do
         web <- liftIO $ readMVar b
         html web

about :: ScottyM ()
about = get "/about" $
  file "./static/pages/about.html"


-- serve main page
home :: ScottyM ()
home =
  get "/" $ authCheck (redirect "/login") $ do
    texts     <- liftIO $ readFile "./static/main.html"
    onlinenum <- getOnlineNum  -- number of people online
    renderMain texts onlinenum

-- disqus page
disqus :: ScottyM ()
disqus = get "/disqus" $ do --login auth
           (uid :: String) <- param "uniqueid"
           text1 <- liftIO $ readFile "./static/main.html"
           text2 <- liftIO $ readFile "./static/js/dis_control.temp"
           let op = case take 1 uid of
                    "w" -> 1
                    "a" -> 2
                    "b" -> 3
                    _   -> 0
           one <- liftIO $ getOneNews op uid
           renderDis text1 text2 one


search :: ScottyM ()
search = get "/search" $ authCheck (text "/login") $ do
  (key :: String) <- param "keyword"
  renderSearch key

chat :: ScottyM ()
chat = get "/chat" $ authCheck (text "/login") $ do
  (name, lev) <- getUserinfo
  if lev > 8
     then do
          text' <- liftIO $ readFile "./static/pages/chat.html"
          json [text', name]
     else json ["no" :: String, ""]


purify :: (MVar CacheList, MVar CacheList, MVar CacheList)
       -> (MVar PageCache, MVar PageCache, MVar PageCache)
       -> ScottyM ()
purify cache page = post "/purify" $ authCheck (text "/login") $ do
  (pid    :: String) <- param "pageid"
  (author :: String) <- param "author"
  (op     :: Int)    <- param "line"
  (_      , lev)     <- getUserinfo
  if lev > 10
     then do liftIO $ when (op > 0 && op < 4) $ do
                           _ <- forkIO $ deletePost (T.pack author) (T.pack pid) op
                           liftIO $ helperDelete pid op cache
                           liftIO $ helperRender (T.pack pid) op cache page notInCache
             text "ok200"
     else text "under construction"


dashboard :: ScottyM ()
dashboard = get "/dashboard" $ authCheck (redirect "/login") $ do
  (name, lev) <- getUserinfo
  text' <- liftIO $ readFile "./static/pages/map.html"
  render' (T.pack name) lev text'

dashOp :: ScottyM ()
dashOp = post "/dash" $ authCheck (text "/login") $ do
  (user :: String) <- param "username"
  (op   :: Int)    <- param "operation"
  (_    , lev)     <- getUserinfo
  if lev > 9
     then do
          case op of
               1 -> liftIO $ setLevel (T.pack user) lev (+1)          -- levelup
               2 -> liftIO $ setLevel (T.pack user) lev (const 0)     -- freeze
               _ -> liftIO $ setLevel (T.pack user) lev (const 8)     -- unfreeze
          redirect "/dashboard"
     else text "/login"


-- serve login
login :: SessionConfig
      -> ScottyM ()
login sess = do
  get "/login" $ do
    salt <- liftIO saltValue
    setSimpleCookie "Lambda" (T.toStrict salt)
    file "./static/login.html"
  post "/login" $ do
    (username :: String) <- param "username"
    (password :: String) <- param "password"
    getuser <- liftIO (authUser username)
    case getuser of
      Nothing ->  text "fail"    -- should return fail information
      Just (Entity _ user) -> do
        salt' <- getCookie "Lambda" -- bad method
        let salt = case salt' of
                     Nothing -> ""
                     Just x  -> T.fromStrict x
        lev  <- liftIO $ getLevel (T.pack username)
        if authPass user salt (T.pack password) && lev > 7
           then do _ <- addSession sess (T.pack username) lev salt
                   text "ok200"
           else    text "fail"


illegal :: String
        -> String
        -> T.Text
illegal email name = if length email < 6 || length name < 6
                        then "email address or username is too short, 6 characters at least"
                        else if any isSpace name
                                then "illegal characters detected!(space in username)"
                                else ""


signup :: ScottyM ()
signup =
  post "/signup" $ do
    (email    :: String) <- param "mail"
    (username :: String) <- param "username"
    (passwor1 :: String) <- param "passwor1"
    (rcode    :: String) <- param "rcode"
    let info = illegal email username
    if not $ T.null info
       then text info
       else if rcode /= "ilovelambda"
               then text "illegal registration code!"
               else do
                    ifUnique <- liftIO $ checkNoUser (T.pack username) (T.pack email)
                    case ifUnique of
                      1 -> text "user exists!"
                      2 -> text "email have been used!"
                      _ -> do
                        liftIO $ addUser (T.pack email) (T.pack username) (T.pack passwor1)
                        text "success"


setting :: ScottyM ()
setting = do
  get "/setting"  $ authCheck (text "/login") $ do
    salt   <- getSalt
    (v, _) <- getUserinfo
    sub    <- liftIO $ getSubscr (T.pack v)
    setSimpleCookie "lambda" (T.toStrict salt)
    setSimpleCookie "name"   (TS.pack v)
    page <- liftIO $ readFile "./static/pages/setting.html"
    json [page, show sub]
  post "/subscr"  $ authCheck (text "/login") $ do
    (n, lev) <- getUserinfo
    liftIO $ when (lev >= 8) $ setSubscr (T.pack n)
    text "ok200"
  post "/setting" $ authCheck (text "/login") $ do
    (passwor1 :: String) <- param "passwor1"
    (passwor2 :: String) <- param "passwor2"
    (passwor3 :: String) <- param "passwor3"
    (subscr   :: Int   ) <- param "subscr"
    (v, _) <- getUserinfo
    when (subscr == 0 || subscr == 1) $ do
      liftIO . setSubscr $ T.pack v
      text "success"
    when (subscr == 1 || subscr == 2) $
      if passwor2 /= passwor3 || passwor1 == passwor2
         then text "illegal password! please check carefully!"
         else do
              getuser <- liftIO (authUser v)
              case getuser of
                Nothing -> text "/login"
                Just (Entity _ value) -> do
                  salt <- getSalt
                  if authPass value salt (T.pack passwor1)
                     then do
                          liftIO $ resetPass (T.pack passwor2) (T.pack v)
                          text "success"
                     else text "current password is wrong!"


starpost :: (MVar CacheList, MVar CacheList, MVar CacheList)
         -> (MVar PageCache, MVar PageCache, MVar PageCache)
         -> ScottyM ()
starpost cache page =
  post "/starpost" $ authCheck (text "please login") $ do
    (pid    :: String) <- param "starpost"
    (op     :: Int)    <- param "line"
    (name, _)          <- getUserinfo
    author             <- liftIO $ getUserById $ T.pack pid
    let v = T.pack name
    ifUnique <- liftIO $ checkNoStar (T.pack pid) v
    if ifUnique
       then do
         _ <- liftIO $ forkIO $ updateStarCount author >>
                                updateStarPost  (T.pack pid) v
         _ <- liftIO $ forkIO $ modifyNews op (T.pack pid)
         liftIO $ helperStar pid op cache
         liftIO $ helperRender (T.pack pid) op cache page inCache
         text "ok200"
       else text "cannot vote again!"


flagpost :: ScottyM ()
flagpost =
  post "/flagpost" $ authCheck (text "please login") $ do
    (title :: String) <- param "title"
    (url :: String)   <- param "url"
    (pid :: String)   <- param "flagpost"
    (name, _) <- getUserinfo
    let v = T.pack name
    ifUnique <- liftIO $ checkNoFlag (T.pack pid) v
    if ifUnique
       then do
         ct <- liftIO getCurrentTime
         liftIO $ updateFlagPost ct
                  (T.pack title)
                  (T.pack url)
                  (T.pack pid)
                  v
         text "ok200"
       else text "cannot flag again!"


deletepost :: (MVar CacheList, MVar CacheList, MVar CacheList)
           -> (MVar PageCache, MVar PageCache, MVar PageCache)
           -> ScottyM ()
deletepost cache page =
  post "/delete" $ authCheck (text "/login") $ do
    (pid :: String) <- param "deletepost"
    (op :: Int) <- param "line"
    (name, _) <- getUserinfo
    let v = T.pack name
    if op == 0
       then do
            liftIO $ deleteFlag v (T.pack pid)
            text "ok200"
       else do
            _ <- liftIO $ forkIO $ deletePost v (T.pack pid) op
            liftIO $ helperDelete pid op cache
            liftIO $ helperRender (T.pack pid) op cache page notInCache
            text "ok200"


-- serve upload img ability
uploadimg :: ScottyM ()
uploadimg =
  post "/upload" $ do
    fs <- files
    let fs' = [(fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName, fi) <- fs]
    liftIO $ sequence_ [B.writeFile ("./imgs" </> fn) fc | (_, fn, fc) <- fs']
    text (head [mconcat ["./" , T.pack fn] | (_, fn, _) <- fs'])


-- serve submit news ability
submitnews :: (MVar CacheList, MVar CacheList, MVar CacheList)
           -> (MVar PageCache, MVar PageCache, MVar PageCache)
           -> ScottyM ()
submitnews cache@(w, b, a) (w', b', a') =
  post "/submitnews" $ authCheck (redirect "/login") $ do
    (title   :: String) <- param "title"
    (rawurl  :: String) <- param "url"
    (img     :: String) <- param "img"
    (intro   :: String) <- param "text"
    (radios  :: Int)    <- param "radios"
    (country :: String) <- param "country"

    (name, _) <- getUserinfo
    let v     = T.pack name
        (op, xs, ys) = case radios of
                       1 -> (News  , w, w')
                       3 -> (Academ, b, b')
                       _ -> (Asks  , a, a')
    -- check if unique
    let uri = if True -- rawurl == "" && radios == 2
                 then return "/#"
                 else do
                      let url' = T.pack $ urlWrapper rawurl
                      ifunique1 <- liftIO $ checkNoNews url' 1  -- world
                      ifunique2 <- liftIO $ checkNoNews url' 2  -- alpha
                      ifunique3 <- liftIO $ checkNoNews url' 3  -- beta
                      if ifunique1 && ifunique2 && ifunique3
                         then return rawurl
                         else return "notunique"
    url <- uri
    if url == "notunique"
       then text "unique url only!"
       else do
            urlReq <- liftIO $ runReq $ urlWrapper url
            if not (urlCheck (fst urlReq))
               then text "illegal url!"
               else do
                    idValue <- liftIO $ updateNewsCount radios
                    let p = map T.pack [title, fst urlReq, snd urlReq, img]
                    liftIO $ insertNews radios v
                               (head p) (p !! 1) (p !! 2) (p !! 3)
                               (T.pack $ renderPlainText intro)
                               idValue
                               cache
                    liftIO $ renderCacheNow xs op ys
                    text "ok200"
                    liftIO $ modifyStat country
                    text'' <- liftIO buildMap
                    liftIO $ writeFile "./static/dist/map/js/data.js" text''


-- serve logout
logout :: ScottyM ()
logout = get "/logout" $ do
    logoutSess
    text "ok200"


-- 404 status
notfound :: ScottyM ()
notfound = notFound $ file "./static/404.html"
