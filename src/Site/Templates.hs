module Site.Templates
  ( templatesP,
    defaultT,
    postT,
    postsT,
    rules,
  )
where

import Data.String.Interpolate.IsString (i)
import Hakyll

templatesP :: Identifier
templatesP = "templates"

defaultT :: Identifier
defaultT = [i|#{templatesP}/default.html|]

postT :: Identifier
postT = [i|#{templatesP}/post.html|]

postsT :: Identifier
postsT = [i|#{templatesP}/posts.html|]

rules :: Rules ()
rules = do
  match [i|#{templatesP}/*|] $ compile templateBodyCompiler
