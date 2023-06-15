module Site.Templates
  ( templatesP,
    defaultT,
    postT,
    archiveT,
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

archiveT :: Identifier
archiveT = [i|#{templatesP}/archive.html|]

rules :: Rules ()
rules = do
  match [i|#{templatesP}/*|] $ compile templateBodyCompiler
