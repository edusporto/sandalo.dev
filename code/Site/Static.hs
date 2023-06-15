module Site.Static (rules) where

import Hakyll
import Data.String.Interpolate.IsString (i)

staticP :: Identifier
staticP = "static"

rules :: Rules ()
rules = do
  match [i|#{staticP}/images/*|] $ do
    route idRoute
    compile copyFileCompiler

  match [i|#{staticP}/css/*|] $ do
    route idRoute
    compile compressCssCompiler
