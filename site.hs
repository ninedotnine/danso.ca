{-# LANGUAGE OverloadedStrings #-}
import Data.List (isSuffixOf)
import Data.Monoid ((<>))
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, UTCTime)
import System.FilePath.Posix (takeBaseName,takeDirectory,takeFileName,(</>))
import Hakyll


timeFormat :: String
timeFormat = "1%Y-%m-%d at %H:%M UTC"
dateFormat :: String
dateFormat = "%B %e, %Y"
-------------------------------------------------------------------------------
main :: IO ()
main = hakyll . hakyllRules =<< getCurrentTime

hakyllRules :: UTCTime -> Rules ()
hakyllRules gentime = do
    let genTimeCtx = constField "gentime" timestring <> defaultContext where 
            timestring = formatTime defaultTimeLocale timeFormat gentime

    match "templates/*" $
        compile templateBodyCompiler
--     match "templates/*" (compile templateCompiler) -- what does this do?

    match "images/*" $ 
        route idRoute >> compile copyFileCompiler

    match "media/**" $
        route idRoute >> compile copyFileCompiler

    match "css/*" $ 
        route idRoute >> compile compressCssCompiler

    match "main/robots.txt" $ 
        route topRoute >> compile copyFileCompiler

    match "main/404.html" $ do
        route topRoute
        compile $ getResourceBody 
                >>= loadAndApplyTemplate "templates/default.html" modTimeCtx

    makeEvents genTimeCtx

    makeBlog genTimeCtx

--     match (fromList ["main/about.rst", "main/contact.md", "main/hacking.md", "main/music.md"]) $ do
    match ("main/*.md") $ do
        route $ customRoute ((</> "index.html") . takeBaseName . toFilePath)
        compile $ pandocCompiler
            >>= theUsual modTimeCtx

    match "music/*" $ do
        route cleanRoute
        compile $ pandocCompiler
            >>= theUsual modTimeCtx

    match "hacking/*" $ do
        route cleanRoute
        compile $ pandocCompiler
            >>= theUsual modTimeCtx

    match "main/index.html" $ do
        route topRoute
        compile $ do
            events <- recentFirst =<< loadAll "events/*"
            let indexCtx = constField "title" "Home" 
                        <> listField "events" dateCtx (return events)
                        <> modTimeCtx
                        <> eventsFeedCtx
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= theUsual indexCtx

-------------------------------------------------------------------------------
makeEvents :: Context String -> Rules ()
makeEvents genTimeCtx = do
    match "events/*" $ do
        route cleanRoute
        compile $ pandocCompiler
            >>= saveSnapshot "event_content"
            >>= loadAndApplyTemplate "templates/event.html" dateCtx
            >>= theUsual (eventsFeedCtx <> modTimeCtx)

    create ["events.html"] $ do
        route cleanRoute
        compile $ do
            events <- recentFirst =<< loadAll "events/*"
            let eventCtx = listField "events" dateCtx (return events)
                            <> constField "title" "Events"
                            <> eventsFeedCtx
                            <> genTimeCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/events.html" eventCtx
                >>= theUsual eventCtx

    create ["feeds/events.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = bodyField "description" <> defaultContext
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "events/*" "event_content"
            renderAtom eventsFeedConf feedCtx posts

makeBlog :: Context String -> Rules ()
makeBlog genTimeCtx = do
    tags <- buildTags "blog/*" (fromCapture "blog/topics/*.html")
    tagsRules tags $ \tag pattern -> do
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let blogCtx = constField "title" ("Posts tagged \"" ++ tag ++ "\"")
                        <> listField "posts" dateCtx (return posts)
                        <> blogFeedCtx
                        <> genTimeCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/postlist.html" blogCtx
                >>= theUsual blogCtx

    create ["blog/topics/index.html"] $ do
        route idRoute
        compile $ do
            topicsList <- renderTagList tags
            topicsCloud <- renderTagCloud 90 150 tags
            let topicsCtx = constField "topicscloud" topicsCloud
                            <> constField "title" "Topics"
                            <> constField "topicslist" topicsList
                            <> blogFeedCtx
                            <> genTimeCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/topics.html" topicsCtx
                >>= theUsual topicsCtx

    match "blog/*" $ do
        let postCtx = tagsField "tags" tags <> dateCtx <> blogFeedCtx
        route cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= theUsual postCtx

    create ["blog.html"] $ do
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/*"
            let blogCtx = listField "posts" dateCtx (return posts)
                        <> constField "title" "Water you thinking about / Aqua penses-tu?"
                        <> blogFeedCtx
                        <> genTimeCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= theUsual blogCtx

    create ["feeds/blog.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<< loadAll "blog/*"
            renderAtom blogFeedConf defaultContext posts

-------------------------------------------------------------------------------
-- contexts
dateCtx :: Context String
dateCtx = dateField "cleandate" dateFormat <> defaultContext

modTimeCtx :: Context String
modTimeCtx = field "modtime" func <> defaultContext where
    func :: Item a -> Compiler String
    func item = do
        modtime <- getItemModificationTime (itemIdentifier item)
        return $ formatTime defaultTimeLocale timeFormat modtime

blogFeedCtx :: Context String
blogFeedCtx = constField "feed" "blog"
            <> constField "feedtitle" (feedTitle blogFeedConf)

eventsFeedCtx :: Context String
eventsFeedCtx = constField "feed" "events"
                <> constField "feedtitle" (feedTitle eventsFeedConf)

-------------------------------------------------------------------------------
-- routes
topRoute :: Routes
topRoute = customRoute (takeFileName . toFilePath)

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute where
    createIndexRoute :: Identifier -> FilePath
    createIndexRoute iden = takeDirectory p </> takeBaseName p </> "index.html"
        where p = toFilePath iden

-------------------------------------------------------------------------------
-- compilers
theUsual :: Context String -> Item String -> Compiler (Item String)
theUsual ctx item = loadAndApplyTemplate "templates/default.html" ctx item
    >>= relativizeUrls
    >>= cleanIndexUrls

-- removes index.html from the ends of links
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex) where
    cleanIndex :: String -> String
    cleanIndex url
        | idx `isSuffixOf` url = take (length url - length idx) url
        | otherwise            = url
      where idx = "index.html"

------------------------------------------------------------------------------
blogFeedConf :: FeedConfiguration
blogFeedConf = FeedConfiguration {
    feedTitle = "Water you thinking about / Aqua penses-tu?",
    feedDescription = "Blog of dan soucy, just like the title says.",
    feedAuthorName = "dan soucy",
    feedAuthorEmail = "contact@danso.ca",
    feedRoot = "https://danso.ca"
}

eventsFeedConf :: FeedConfiguration
eventsFeedConf = FeedConfiguration {
    feedTitle = "Events",
    feedDescription = "Events with dan soucy.",
    feedAuthorName = "dan soucy",
    feedAuthorEmail = "contact@danso.ca",
    feedRoot = "https://danso.ca"
}
