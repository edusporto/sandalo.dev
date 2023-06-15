module Site.Pages (rules) where

import Data.String.Interpolate.IsString (i)
import Hakyll
import Site.Posts (postCtx, postsP)
import Site.Templates qualified as Templates

pagesP :: Identifier
pagesP = "pages"

pagesCustomP :: Identifier
pagesCustomP = "custom"

rules :: Rules ()
rules = do
  normalPages
  archivePage
  indexPage

indexPage :: Rules ()
indexPage =
  match [i|#{pagesCustomP}/index.html|] $ do
    route $ gsubRoute (show pagesCustomP) (const ".")
    compile $ do
      posts <- recentFirst =<< loadAll [i|#{postsP}/*|]
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate Templates.defaultT indexCtx
        >>= relativizeUrls

archivePage :: Rules ()
archivePage =
  -- create [[i|#{pagesP}/archive.html|]] $ do
  create [[i|archive.html|]] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll [i|#{postsP}/*|]
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate Templates.archiveT archiveCtx
        >>= loadAndApplyTemplate Templates.defaultT archiveCtx
        >>= relativizeUrls

normalPagesList :: [Identifier]
normalPagesList =
  [ [i|#{pagesP}/about.rst|],
    [i|#{pagesP}/contact.md|]
  ]

normalPages :: Rules ()
normalPages =
  match (fromList normalPagesList) $ do
    -- route $ setExtension "html" <> gsubRoute (show pagesP) (const "test")
    route $ gsubRoute (show pagesP) (const ".") `composeRoutes` setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate Templates.defaultT defaultContext
        >>= relativizeUrls
