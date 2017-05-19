{-# LANGUAGE OverloadedStrings          #-}
module Skeleton.Kernel.Core.Mail(
       mailLoop
       ) where

import  Skeleton.Kernel.Internal.Model
import  Skeleton.Kernel.Internal.Type
import  Skeleton.Kernel.Account
import  Database.Persist

import  Network.HaskellNet.SMTP.SSL
import  Skeleton.Kernel.Core.Cache
import  Skeleton.Kernel.Core.Sort
import  Control.Concurrent                    (readMVar, MVar)
import  Network.Mail.Mime
import  Data.List                             (sortOn)
import  Text.Printf                           (printf)
import  qualified Data.Text as DT
import  qualified Data.Text.Lazy as T
import  qualified Data.ByteString as S
import  qualified Data.ByteString.Lazy as B


getValue' :: DataEle
          -> EleM
getValue' (_, a, b, _, c, _, _, _, _, d) = (T.unpack a, T.unpack b,
                                            T.unpack c, d)

getValue :: [DataEle]
         -> ListOfPostM
getValue x = sortM $ map getValue' x

render' :: (MVar CacheList, MVar CacheList, MVar CacheList)
        -> IO (ListOfPostM, ListOfPostM, ListOfPostM)
render' (w, b, a) = do
        w <- readMVar w
        a <- readMVar a 
        b <- readMVar b
        let news = getValue w
            acad = getValue b
            asks = getValue a
        return (news, acad, asks)


renderHtml' :: EleM
            -> String
renderHtml' (id, title, host, star) = printf
                                      "<tr>\
                                       \<td>%d</td>\
                                       \<td><a href='http://sapphiresoft.io/disqus?uniqueid=%s'> %s </a></td>\
                                       \<td>%s</td>\
                                       \</tr>"
                                       star id title host
            

renderTable :: String
renderTable ="<table class='table'>\
                \<thead>\
                   \<tr>\
                     \<th>Star</th>\
                     \<th>Title</th>\
                     \<th>Host</th>\
                   \</tr>\
                \</thead><tbody>"


renderHtmlBody :: ListOfPostM
               -> ListOfPostM
               -> ListOfPostM
               -> T.Text
renderHtmlBody w b a = T.pack $
                       "<h2> World </h2>"    ++ renderTable ++ 
                       (foldl (++) "" $  map renderHtml' w) ++ "</tbody></table>" ++ 
                       "<h2> Academic </h2>" ++ renderTable ++ 
                       (foldl (++) "" $  map renderHtml' b) ++ "</tbody></table>" ++ 
                       "<h2> Ask </h2>"      ++ renderTable ++ 
                       (foldl (++) "" $  map renderHtml' a) ++ "</tbody></table>"
                       


send :: T.Text
     -> [String]
     -> IO ()
send mailBody to = doSMTPSSL "your smtp server here" $ \conn -> do
  authSucceed <- authenticate PLAIN "your emailaddr here" "your pass here" conn
  if authSucceed
     then do
          let newMail = Mail (Address (Just "Lambda") "support@sapphiresoft.io")
                             (renderAddr to)
                             []
                             []
                             [("Subject", "Weekly • λ - Top Star")]
                             [[plainPart "", htmlPart mailBody]]
          newMail'     <- addAttachments [] newMail
          renderedMail <- renderMail' newMail'
          sendMail "support@sapphiresoft.io"
                   to
                   (S.concat . B.toChunks $ renderedMail)
                   conn
     else print "Authentication failed."
  where
    renderAddr' addr = Address Nothing (DT.pack addr)
    renderAddr  []   = []
    renderAddr  xs   = map renderAddr' xs 


mailLoop :: (MVar CacheList, MVar CacheList, MVar CacheList)
         -> IO () -- weekly
mailLoop cache = do
  (w, b, a) <- render' cache
  mailList  <- getMailAddr
  send (renderHtmlBody w b a)
       mailList
