module Site.Pages (rules) where

import Data.String.Interpolate.IsString (i)
import Hakyll
import Site.Posts (postCtx, postsP)
import Site.Templates qualified as Templates

pagesP :: Identifier
pagesP = "pages"

pagesCustomP :: Identifier
pagesCustomP = [i|#{pagesP}/custom|]

rules :: Rules ()
rules = do
  normalPages
  postsPage
  indexPage

indexPage :: Rules ()
indexPage =
  match [i|#{pagesCustomP}/index.html|] $ do
    route $ gsubRoute (show pagesCustomP) (const ".")
    compile $ do
      posts <- recentFirst =<< loadAll [i|#{postsP}/*|]
      let indexCtx =
            listField "posts" postCtx (return posts) <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate Templates.defaultT indexCtx
        >>= relativizeUrls

postsPage :: Rules ()
postsPage =
  create [[i|posts.html|]] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll [i|#{postsP}/*|]
      let archiveCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "Posts"
              <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate Templates.postsT archiveCtx
        >>= loadAndApplyTemplate Templates.defaultT archiveCtx
        >>= relativizeUrls

normalPages :: Rules ()
normalPages =
  match [i|#{pagesP}/*|] $ do
    route $ gsubRoute (show pagesP) (const ".") `composeRoutes` setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate Templates.defaultT defaultContext
        >>= relativizeUrls
