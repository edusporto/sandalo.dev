module Site.Posts (postCtx, postsP, rules) where

import Data.String.Interpolate.IsString (i)
import Hakyll
import Site.Templates qualified as Templates

postsP :: Identifier
postsP = "posts"

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <> defaultContext

rules :: Rules ()
rules = do
  match [i|#{postsP}/*/*.md|] $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate Templates.postT postCtx
        >>= loadAndApplyTemplate Templates.defaultT postCtx
        >>= relativizeUrls
  match [i|#{postsP}/*/*|] $ do
    route idRoute
    compile copyFileCompiler
