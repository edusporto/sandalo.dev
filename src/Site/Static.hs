module Site.Static (rules) where

import Compiler (pandocCodeStyle, relativizeFuncField)
import Data.String.Interpolate.IsString (i)
import Hakyll
import Text.Pandoc.Highlighting (styleToCss)

staticP :: Identifier
staticP = "static"

rules :: Rules ()
rules = do
  match [i|#{staticP}/images/*|] $ do
    route idRoute
    compile copyFileCompiler

  match [i|#{staticP}/css/*|] $ do
    route idRoute
    compile $ compressCssCompiler >>= applyAsTemplate (relativizeFuncField <> defaultContext)

  create [[i|#{staticP}/css/style.css|]] $ do
    route idRoute
    compile $ do
      makeItem $ styleToCss pandocCodeStyle

  match [i|#{staticP}/js/*|] $ do
    route idRoute
    compile copyFileCompiler

  match [i|#{staticP}/fonts/*/*|] $ do
    route idRoute
    compile copyFileCompiler
