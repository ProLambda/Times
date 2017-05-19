{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skeleton.Shell.Template (
         renderCacheNow
       , renderCache
       , renderMain
       , render'
       , renderSearch
       , renderDis
       , renderPlainText
       )where

import  Skeleton.Kernel.Post

import  Skeleton.Kernel.Core.Cache
import  Control.Concurrent             (MVar, readMVar)
import  Control.Monad                  (forM_)

import  Skeleton.Kernel.Account
import  Database.Persist
import  Skeleton.Kernel.Internal.Model
import  qualified Data.Text      as T
import  qualified Data.Text.Lazy as TL
import  Control.Monad.IO.Class
import  Data.Time.Clock
import  Data.List                      (isInfixOf)
import  Web.Scotty                     hiding (post)
import  Text.Printf                    (printf)
import  Text.Blaze                     (toValue)
import  Text.Blaze.Html5               hiding (time, html, map, style, head, title, 
                                               b, a, img, i)
import  Text.Blaze.Internal            (preEscapedText)
import  Text.Blaze.Html5.Attributes    hiding (content, list, name, title)
import  Text.Blaze.Html.Renderer.Text  (renderHtml)
import  Prelude                        hiding (div, id)
import  Skeleton.Kernel.Core.Helper    hiding (week)
import  Skeleton.Kernel.Internal.Type
import  Skeleton.Kernel.Core.Sort

-------------------------------------------------------------
-- init
-------------------------------------------------------------

pET :: T.Text -> Markup
pET = preEscapedText

pageLen :: Int
pageLen = 20 -- 20 news per page

-------------------------------------------------------------
-- utlis
-------------------------------------------------------------

navTab :: String
       -> String
       -> Html
navTab title line =
       pET $ T.pack $ printf 
         "<ul id=\"navtab%s\" class=\"nav nav-tabs\"> \
            \<li class=\"active\"><a href=\"#home%s\" data-toggle=\"tab\" aria-expanded=\"true\">\
              \<span class='fa fa-sort-numeric-asc'></span> Top %s \
              \</a></li>\
            \<li class=\"\"><a href=\"#profile%s\" data-toggle=\"tab\" aria-expanded=\"false\">  \
              \<span class='fa fa-list-ol'></span> Latest %s \
              \</a></li>\
            \<li class=\"\"><a href=\"#\" onclick=\"refresh(\'%s\');\">\
              \<span class='fa fa-refresh fa-spin'></span> Refresh\
              \</a></li>\
           \</ul>" 
         line line title line title title 

adminDash :: String
adminDash = "<li id='dash_admin'><a href='#admin' data-toggle='tab' aria-expanded='false'>\
             \<span class='fa fa-users'></span> Admin</a></li></ul>"

navTabDash :: Bool
           -> Html
navTabDash op = pET $ T.pack ("<ul id='navtab-dash' class='nav nav-tabs'>\
             \<li id='dash_home' class='active'><a href='#myhome' data-toggle='tab' aria-expanded='true'>\
             \<span class='fa fa-list-alt'></span> My Issue </a></li>\
             \<li id='dash_flag'><a href='#flag' data-toggle='tab' aria-expanded='false'>\  
             \<span class='fa fa-flag'></span> Flag </a></li>\
             \<li id='dash_stat'><a href='#stat' data-toggle='tab' aria-expanded='false'>\
             \<span class='fa fa-bar-chart'></span> Stat</a>\
             \</li>" ++ if op then adminDash else "</ul>")

paginate   :: Int 
           -> Int
           -> String
           -> String
paginate   _ 0 _ = ""
paginate   c t s | c == t = (++) (paginate c (t - 1) s) 
                                 (printf "<li id=\"pag_%s%d\" class=\"active\"><a onclick=\"displaypage(\'%s\', %d)\">%d</a></li>"
                                         s c s c c)
                 | otherwise = (++) (paginate c (t - 1) s) 
                                    (printf "<li id=\"pag_%s%d\"><a onclick=\"displaypage(\'%s\', %d);\">%d</a></li>"
                                            s t s t t)

pagination :: Int       -- current page
           -> Int       -- total page
           -> String    -- Top/All; Myissue/Flag
           -> String
pagination c t s = printf 
                   "<ul class=\"pagination\"> \
                   \<li id=\"topest%s\" class=\"disabled\"><a onclick=\"displaypage(\'%s\', 1);\">|‹</a></li> \
                   \<li id=\"previous%s\" class=\"disabled\"><a onclick=\"previous(\'%s\');\">&laquo;</a></li>\
                   \ %s\
                   \<li id=\"next%s\" class=\"%s\"><a onclick=\"next(\'%s\');\">&raquo;</a></li>\
                   \<li id=\"endest%s\" class=\"%s\"><a onclick=\"displaypage(\'%s\', %d);\">›|</a></li></ul>\
                   \<script>\
                     \var page%sLen = %d;\
                   \</script>"
                   s s s s
                   (paginate c t s) 
                   s ((\x -> if x <= 1 then "disabled" else ("" :: String)) t)
                   s s ((\x -> if x <= 1 then "disabled" else ("" :: String)) t) 
                   s t s t

helper :: String 
       -> Int
       -> String
helper b x = (++) b $ show x
                      
accordOrd :: Int
          -> Int
          -> String
          -> [String]
accordOrd 0 _ _ = []
accordOrd n s b | n < 0 = []
                | n < pageLen = map (helper b) $ take n (map (+ s) [1, 1 ..])
                | otherwise = (map (helper b) $ take pageLen (map (+ s) [1, 1 ..])) ++
                              (accordOrd (n - pageLen) (s + 1) b)
                  

trans :: String
      -> [Int]
      -> [String]
trans x xs = map (helper x) xs


getValueAd :: [Entity User]
           -> String
           -> ListOfPost''
             -- sort before zip
getValueAd x b = zip (zip (trans b [(1 :: Int)..]) (accordOrd (length x) 0 b)) $ map getValueAd' x


getValueS :: [EleS]
          -> ListOfPostS
             -- sort before zip
getValueS x = zip (map show [(1 :: Int)..]) x


getValueN :: [Entity NewsPost]
          -> String
          -> ListOfPost'
             -- sort before zip
getValueN x b = zip (zip (trans b [(1 :: Int)..]) (accordOrd (length x) 0 b)) $ map getValueN' x

getValueF :: [Entity FlagPost]
          -> String
          -> ListOfPost'
             -- sort before zip
getValueF x b = zip (zip (trans b [(1 :: Int)..]) (accordOrd (length x) 0 b)) $ map getValueF' x


getValue :: CacheList
         -> String
         -> ListOfPost
         -- sort before zip
getValue x b = zip (zip (trans b [(1 :: Int)..]) (accordOrd (length x) 0 b)) $ map getValue' x



genPanelHtml' :: ListOfPost
              -> String
              -> UTCTime
              -> [Html]
genPanelHtml' xs line ctime = [panelLayout
                               no aid uid t week u h s at (getTimeDiff ctime ti) im it line |
                               ((no, aid), (at, uid, t, u, h, im, it, ti, week, s)) <- xs]

genPaneladmin :: ListOfPost''
              -> [Html]
genPaneladmin xs = [panelEle' no mail name star lev |
                    ((no, _), (mail, name, star, lev)) <- xs]


genPaneldash :: ListOfPost'
             -> Bool
             -> [Html]
genPaneldash xs ifflag = [panelEle no time title url page ifflag |
                          ((no, _), (time, title, url, page)) <- xs]

genPanelS :: ListOfPostS
          -> [Html]
genPanelS xs = [panelEleS no author pid title url time star |
                (no, (author, pid, title, url, time, star)) <- xs]


genPanelHtml :: Int
             -> [Html]
             -> [[Html]]
genPanelHtml _ [] = []
genPanelHtml len xs = take len xs : genPanelHtml len (drop len xs)

-------------------------------------------------------------
-- render '/' and '/disqus'
-------------------------------------------------------------

disqusHelper :: String
             -> Ele
             -> String
disqusHelper org (_, pid, title, url, _, _, intro, _, (_, week) ,_) =
             printf
               (org :: String)
               title pid pid week ("DISQUS" :: String) title intro
               pid line url
               where
                 line = case (take 1 pid) of
                             "w" -> ("World" :: String)
                             "a" -> "Alpha"
                             _   -> "Beta"


renderDis :: String
          -> String
          -> Ele
          -> ActionM ()
renderDis text1 text2 one =
          html $ TL.pack $ printf
            text1
            ("<li><a href=\"/\"><span class='fa fa-space-shuttle'></span> 世界线跃迁</a></li>" :: String)
            (-1 :: Int)
            ((disqusHelper text2 one) :: String)


renderMain :: String
           -> Int
           -> ActionM ()
renderMain texts online = html $ TL.pack $ printf
           texts
           ("<li><a class='page-scroll' href=\"#world\"><span class='fa fa-globe'></span> World</a></li>\
            \<li><a class='page-scroll' href=\"#beta\"><span class='fa fa-fort-awesome'></span> β•Line</a></li>\
            \<li><a class='page-scroll' href=\"#alpha\"><span class='fa fa-quora'></span> α•Line</a></li>" :: String)
           online
           ("<script src=\"./js/main.js\"></script>\
            \<script src=\"./js/dis.js\"></script>" :: String)

-------------------------------------------------------------
-- panel component
-------------------------------------------------------------

panelEle'  :: String
           -> String
           -> String
           -> Int
           -> Int
           -> Html
panelEle'  no mail name star lev = docTypeHtml $ do
            pET $ T.pack $ printf
              "<li class='list-group-item'>\
                 \<span style='cursor: pointer;' class='badge' onclick=\"dashOP('%s', 3);\"><span class='fa fa-unlock'/> UnFreeze</span>\
                 \<span style='cursor: pointer;' class='badge' onclick=\"dashOP('%s', 2);\"><span class='fa fa-lock'/> Freeze</span>\
                 \<span style='cursor: pointer;' class='badge' onclick=\"dashOP('%s', 1)\"><span class='fa fa-level-up'/> Levelup</span>\
                 \ %s. Username: %s | Mail: %s | Star: %d | Level:%d\
                 \</li>"
              name name name (drop 1 no) name mail star lev


panelEle  :: String
          -> UTCTime
          -> String
          -> String
          -> String
          -> Bool   -- line 
          -> Html
panelEle  no time title url page l = docTypeHtml $ do
            pET $ T.pack $ printf
              "<li class='list-group-item'>\
                 \<span style='cursor: pointer;' class='badge' onclick=\"deletePost('%s', '%d');\"><span class='fa fa-trash-o'/> delete</span>\
                 \<span style='cursor: pointer;' class='badge' onclick=\"location.href='%s';\"><span class='fa fa-comments-o'/> disqus</span>\
                 \ %s. %s | <a href='%s'>%s</a> \
                 \</li>"
              page (line l :: Int) ("/disqus?uniqueid=" ++ page) (drop 1 no) (take 16 $ show time) url title 
              where
                line l' = if l' then 0
                                else case (take 1 page) of
                                          "w" -> 1
                                          "a" -> 2
                                          _   -> 3


panelEleS :: String
          -> String
          -> String
          -> String
          -> String
          -> UTCTime
          -> Int 
          -> Html
panelEleS no author pid title url time star = docTypeHtml $ do
            pET $ T.pack $ printf
              "<li class='list-group-item'>\
                 \<span class='badge'>%d <span class='fa fa-star-o'/></span>\
                 \<span style='cursor: pointer;' class='badge' onclick=\"location.href='%s';\"><span class='fa fa-comments-o'/> disqus</span>\
                 \ %s. %s | <a href='%s'>%s</a> by %s\
                 \</li>"
              star ("/disqus?uniqueid=" ++ pid) no (take 16 $ show time) url title author


panelTitle :: String
           -> String
           -> String
           -> String
           -> String
           -> String
           -> String
           -> Int
           -> String
           -> String
           -> Html
panelTitle no aid line newid title date host stars author difftime = docTypeHtml $ do
           pET $ T.pack $ printf
             "<div class=\"panel panel-default\">\
                \<div class=\"panel-heading\" role=\"tab\" id=\"heading%s\">\
                  \<h4 class=\"panel-title\">\
                    \<a data-toggle=\"collapse\" data-parent=\"#accordion%s\" href=\"#collapse%s\" \
                      \aria-expanded=\"false\" aria-controls=\"collapse%s\">\
                      \ %s. %s <small> (%s | %s) </small>\n\
                      \<br>\n\
                      \<small><span class='fa fa-fire'></span> %d | \
                        \posted by %s %s | <span class='fa fa-comments'></span> \
                        \<span class='disqus-comment-count' \
                        \data-disqus-identifier='%s'>comment</span> </small>\
                      \</a>"
             no' aid' no' no' (num no) title date host stars author difftime newid
           where
             num x = if head x == 'T' 
                        then drop 4 x
                        else drop 5 x
             no'   = line ++ no
             aid'  = line ++ aid


panelBody :: String
          -> String
          -> String
          -> String
          -> String
          -> String
          -> String
          -> String
          -> String
          -> Html
panelBody no newid img intro url title date line author = docTypeHtml $ do
          pET $ T.pack $ printf
            "<div id=\"collapse%s\" class=\"panel-collapse collapse\" role=\"tabpanel\" aria-labelledby=\"heading%s\">\
              \<div class=\"panel-body\">\
                \<div class=\"row featurette\">\
                  \<div class=\"col-md-2\">\
                    \<img class=\"featurette-image img-responsive img-rounded\" data-src=\"holder.js/200x200/auto\" \
                      \alt=\"200x200\" src=\"%s\" data-holder-rendered=\"true\"></div>\
                  \<div class=\"col-md-10\">\
                    \<p class='lead'> %s </p>\
                    \<p><a class=\"btn btn-default\" onclick=\"starpost('%s', '%s');\" role=\"button\" \
                        \data-toggle=\"tooltip\" data-placement=\"bottom\" data-original-title=\"+1 point\">\
                        \<span class='fa fa-thumbs-o-up'></span> +1</a> \
                        \<a class=\"btn btn-default\" href=\"%s role=\"button\"\
                        \data-toggle=\"tooltip\" data-placement=\"bottom\" title=\"Open the original link\">\
                        \<span class='fa fa-external-link'></span> Open</a> \
                        \<a class=\"btn btn-default\" \
                          \onclick=\"disqussPage('%s','%s','%s','"
            no' no' img intro newid line url' newid date line
          string title -- escape string
          pET    "','"
          string intro
          pET $ T.pack $ printf
                 "','%s');\" role=\"button\"\
                        \data-toggle=\"tooltip\" data-placement=\"bottom\" title=\"Disqus: using VPN if you cannot access\">\
                        \<span class='fa fa-comments-o'></span> Views</a> \ 
                        \<a class=\"btn btn-default\" onclick=\"flagpost('%s','%s','%s');\" role=\"button\" \
                        \data-toggle=\"tooltip\" data-placement=\"bottom\" title=\"Flag it\"\
                        \><span class='fa fa-flag-o'></span> Flag</a> \
                        \<a class=\"btn btn-default\" onclick=\"purify('%s','%s','%s');\" role=\"button\"\
                        \data-toggle=\"tooltip\" data-placement=\"bottom\" title=\"Report trash content\">\
                        \<span class=\"fa fa-trash-o\"></span> PURIFY</a> </p>"
            url title url newid newid author line
          where
            url' = if (url == "/#") then url ++ "\""
                                    else url ++ "\" target='_blank'"
            no'  = line ++ no
       

panelLayout :: String
            -> String
            -> String
            -> String
            -> (String, String)
            -> String
            -> String
            -> Int
            -> String
            -> String
            -> String
            -> String
            -> String
            -> Html
panelLayout no aid pid title date url host stars author difftime img intro line = docTypeHtml $ do
            panelTitle no aid line pid title (fst date) host stars author difftime
            pET "</h4></div>"
            panelBody no pid img intro url title (snd date) line author
            pET "</div></div></div></div></div>"



panelGroup'' :: ListOfPost''
             -> Int
             -> Html
panelGroup'' ones ifissue = docTypeHtml $ do
            let panelHtml = genPaneladmin ones
                totalPage = genPanelHtml pageLen panelHtml
                lenPage   = length totalPage
                ifissue'  = show ifissue
            forM_ (zip [(1 :: Int) ..] totalPage) $ \i -> do
              let fi = fst i
              pET $ T.pack $ printf
                "<ul class='list-group' id='accordion%s%d' style='display:%s;'>"
                ifissue' fi ((\x -> if x < 2 then ("block" :: String) else "none") fi)
              forM_ (snd i) $ \j -> j
              pET "</ul>"
            pET $ T.pack $ pagination 1 lenPage ifissue'


panelGroup' :: ListOfPost'
            -> Int
            -> Bool
            -> Html
panelGroup' news ifissue ifflag = docTypeHtml $ do
            let panelHtml = genPaneldash news ifflag
                totalPage = genPanelHtml pageLen panelHtml
                lenPage   = length totalPage
                ifissue'  = show ifissue
            forM_ (zip [(1 :: Int) ..] totalPage) $ \i -> do
              let fi = fst i
              pET $ T.pack $ printf
                "<ul class='list-group' id='accordion%s%d' style='display:%s;'>"
                ifissue' fi ((\x -> if x < 2 then ("block" :: String) else "none") fi)
              forM_ (snd i) $ \j -> j
              pET "</ul>"
            pET $ T.pack $ pagination 1 lenPage ifissue'


panelGroupS :: ListOfPostS
            -> Html
panelGroupS news = docTypeHtml $ do
            let searchHtml = genPanelS news 
                totalPage  = genPanelHtml pageLen searchHtml
                lenPage    = length totalPage
            forM_ (zip [(1 :: Int) ..] totalPage) $ \i -> do
              let fi = fst i
              pET $ T.pack $ printf
                "<ul class='list-group' id='accordionS%d' style='display:%s;'>"
                fi ((\x -> if x < 2 then ("block" :: String) else "none") fi)
              forM_ (snd i) $ \j -> j
              pET "</ul>"
            pET $ T.pack $ pagination 1 lenPage "S"


panelGroup :: ListOfPost
           -> String
           -> UTCTime
           -> Bool
           -> Html
panelGroup news line ctime ifTop = docTypeHtml $ do
           let panelHtml = genPanelHtml' news line ctime
               totalPage = genPanelHtml pageLen panelHtml
               lenPage   = length totalPage
               iftop     = show ifTop
           forM_ (zip [(1 :: Int) ..] totalPage) $ \i -> do
             let fi = fst i
             pET $ T.pack $ printf
               "<div class=\"panel-group\" id=\"accordion%s%s%d\"\
                \role=\"tablist\" aria-multiselectable=\"true\" style=\"display:%s;\">"
               line iftop fi ((\x -> if x < 2 then ("block" :: String) else "none") fi)
             forM_ (snd i) $ \j -> j
             pET "</div>"
           pET $ T.pack $ pagination 1 lenPage (line ++ iftop)



-------------------------------------------------------------
-- build world line
-------------------------------------------------------------

issueOnes :: ListOfPost'
          -> Html
issueOnes ones = docTypeHtml $ do
        div ! class_ "tab-pane active" ! id "myhome" $ 
            panelGroup' ones 0 False
                     
flagOnes :: ListOfPost'
         -> Html
flagOnes ones = docTypeHtml $ do
         div ! class_ "tab-pane" ! id "flag" $ do
             panelGroup' ones 1 True

adminOnes :: ListOfPost''
          -> Html
adminOnes ones = docTypeHtml $ do
        div ! class_ "tab-pane" ! id "admin" $ 
            panelGroup'' ones 2

searchOnes :: ListOfPostS
           -> Html
searchOnes ones = docTypeHtml $ do
           div ! id "searchOnes" $ do
             panelGroupS ones


topOnes :: ListOfPost 
        -> String
        -> UTCTime
        -> Html
topOnes ones line ctime = docTypeHtml $ do
        div ! class_ "tab-pane active" ! id (toValue $ "home" ++ line) $ 
          panelGroup ones line ctime True
          
allOnes :: ListOfPost
        -> String
        -> UTCTime
        -> Html
allOnes ones line ctime = docTypeHtml $ do
        div ! class_ "tab-pane" ! id (toValue $ "profile" ++ line) $ do
          panelGroup ones line ctime False


level :: Int
      -> String
level lev = case lev of
                 8 -> "情报Hacker"
                 9 -> "特A级Hacker"
                 10 -> "超S级Hacker"
                 11 -> "首席管理员"
                 _ -> "虚空Hacker:你是人才，请联系我们"


layout' :: ListOfPost'
        -> ListOfPost'
        -> ListOfPost''
        -> Int
        -> Int
        -> String
        -> Html
layout' ones ones' ones'' star lev map' = docTypeHtml $ do
       div ! class_ "tab-content" ! id "content-tab" $ do
         issueOnes ones
         flagOnes ones'
         let lenIss = length ones
             lenFla = length ones'
         div ! class_ "tab-pane" ! id "stat" $ do
           pET $ T.pack $ printf "<div class='panel panel-info'>\
                   \<div class='panel-heading'>\
                     \<h3 class='panel-title'>Statistics</h3>\
                   \</div>\
                   \<div class='panel-body'>\
                     \ %d <span class='fa fa-newspaper-o'/> Posts | \
                     \ %d <span class='fa fa-flag-checkered'/> Flags | \
                     \ %d <span class='fa fa-star'/> Stars | \
                     \ <span class='fa fa-user-secret'/> %s\
                   \</div>\
                   \</div>"
                  lenIss lenFla star (level lev)
           pET $ T.pack map'
         if lev > 9 then adminOnes ones''
                    else pET ""
       pET "<script src='./js/admin.js'></script>"

layoutSearch :: ListOfPostS
             -> Html
layoutSearch ones  = docTypeHtml $ do
             pET "<blockquote style='margin: 10px auto;'><h4> 檢索 • λ 計劃 </h4></blockquote>"
             searchOnes ones 

layout :: ListOfPost
       -> ListOfPost
       -> String
       -> UTCTime
       -> Html
layout ones ones' line ctime = docTypeHtml $ do
       div ! class_ "tab-content" ! id "content-tab" $ do
         topOnes ones line ctime
         allOnes ones' line ctime
       pET "<script>$(function () {$(\"[data-toggle='tooltip']\").tooltip();});</script>"
       

-------------------------------------------------------------
-- render
-------------------------------------------------------------

renderPlainText :: String
                -> String
renderPlainText [] = []
renderPlainText (x:xs) | x == '\n' = (++) "<br>" (renderPlainText xs)
                       | x == '\r' = renderPlainText xs 
                       | otherwise = x : renderPlainText xs


getUser' :: Int
         -> IO ListOfPost''
getUser' admin = do
  user <- liftIO $ getUser admin
  return $ getValueAd (reverse user) "2"


getNews' :: TL.Text
         -> IO ListOfPost'
getNews' name = do
  news <- liftIO $ getNews name
  return $ getValueN (reverse news) "0"


getFlag' :: TL.Text
         -> IO ListOfPost'
getFlag' name = do
  flag <- liftIO $ getFlag name
  return $ getValueF (reverse flag) "1"



render' :: TL.Text
        -> Int
        -> String
        -> ActionM ()
render' name lev map' = do
  news <- liftIO $ getNews' name
  flag <- liftIO $ getFlag' name
  star <- liftIO $ getStars name
  user <- liftIO $ getUser' lev
  let content = layout' news flag user star lev map'
  html . renderHtml $ (navTabDash $ lev > 9) >> content


filterSearch :: String
             -> [Ele]
             -> [Ele]
filterSearch key list = filter (eleS key) list
             where
                eleS key' (a, _, b, _, _, _, c, _, _, _) = (key' `isInfixOf` a) ||
                                                           (key' `isInfixOf` b) ||
                                                           (key' `isInfixOf` c)


searchW :: String
        -> IO [EleS]
searchW key = do
  news <- liftIO $ getWorldNews
  return $ map getValueS' $ filterSearch key (map getValueW' news)

searchA :: String
        -> IO [EleS]
searchA key = do
  news <- liftIO $ getAlphaNews 
  return $ map getValueS' $ filterSearch key $ map getValueA' news

searchB :: String
        -> IO [EleS]
searchB key = do
  news <- liftIO $ getBetaNews 
  return $ map getValueS' $ filterSearch key $ map getValueB' news 
 

renderSearch :: String
             -> ActionM ()
renderSearch key = do
  world <- liftIO $ searchW key
  alpha <- liftIO $ searchA key
  beta <- liftIO $ searchB key
  let res = getValueS (world ++ alpha ++ beta)
      content = layoutSearch res
  html . renderHtml $ content
  

render :: MVar CacheList
       -> TypePost -- which one
       -> IO TL.Text
render cache op = do
       -- read the disqus script in file
       news <- liftIO $ readMVar cache
       ctime <- liftIO $ getCurrentTime
       let news' = getValue news "False"
           temp = unzip $ getValue news "True"
           head = case op of
                       News   -> ("News"    , "World")
                       Asks   -> ("Ask"     , "Alpha")
                       Academ -> ("Academic", "Beta" )
           content = layout (zip (fst temp) (sort ctime $ snd temp)) news' (snd head) ctime
       return . renderHtml $ navTab (fst head) (snd head) >> content


-------------------------------------------------------------
-- temp cache
-------------------------------------------------------------

renderCacheNow :: MVar CacheList
               -> TypePost
               -> MVar PageCache
               -> IO ()
renderCacheNow cache op page = do
  web <- render cache op
  renderCacheNow' page web
  print "Log: now: rendered!"
  

renderCache :: [MVar CacheList]
            -> [TypePost]
            -> [MVar PageCache]
            -> IO ()
renderCache caches ops pages = do
  let webs = [render a b | (a, b) <- (zip caches ops)]
  renderCache' pages webs
  print "Log: rendered!"
