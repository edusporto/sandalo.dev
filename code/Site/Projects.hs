module Site.Projects (projectsP, rules) where

import Data.String.Interpolate.IsString (i)
import Hakyll

projectsP :: Identifier
projectsP = "projects"

rules :: Rules ()
rules = do
  match [i|#{projectsP}/*/*|] $ do
    route idRoute
    compile copyFileCompiler
