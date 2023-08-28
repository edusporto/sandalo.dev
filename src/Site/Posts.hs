{-# LANGUAGE NamedFieldPuns #-}

module Site.Posts (postCtx, postsP, rules, RenderFormulae) where

import Compiler (myPandocCompilerWithTransformM)
import Data.String.Interpolate.IsString (i)
import Hakyll
import Image.LaTeX.Render
import Image.LaTeX.Render.Pandoc
import Site.Templates qualified as Templates
import Text.Pandoc

type RenderFormulae = PandocFormulaOptions -> Pandoc -> Compiler Pandoc

postsP :: Identifier
postsP = "posts"

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <> defaultContext

pandocFomulaOptions :: PandocFormulaOptions
pandocFomulaOptions =
  defaultPandocFormulaOptions
    { shrinkBy = 1,
      formulaOptions = options
    }
  where
    options mathType =
      let op@FormulaOptions {preamble} = formulaOptions defaultPandocFormulaOptions mathType
       in op
            { dpi = 200,
              preamble =
                preamble
                  <> "\\usepackage{color}\\usepackage{xcolor}\\usepackage{everysel}\\usepackage{tikz}"
                  <> "\\usetikzlibrary{automata,positioning,patterns}"
                  -- <> "\\EverySelectfont{\\color{red}}"
                  <> "\\everymath{\\color[HTML]{888888}}"
                  <> "\\everydisplay{\\color[HTML]{888888}}"
            }

rules :: RenderFormulae -> Rules ()
rules renderFormulae = do
  match [i|#{postsP}/**.md|] $ do
    route $ setExtension "html"
    compile $
      myPandocCompilerWithTransformM
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        (renderFormulae pandocFomulaOptions)
        >>= loadAndApplyTemplate Templates.postT postCtx
        >>= loadAndApplyTemplate Templates.defaultT postCtx
        >>= relativizeUrls

  match [i|#{postsP}/**|] $ do
    route idRoute
    compile copyFileCompiler
