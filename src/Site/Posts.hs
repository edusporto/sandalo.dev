{-# LANGUAGE NamedFieldPuns #-}

module Site.Posts (postCtx, postsP, rules, RenderFormulae) where

-- import Compiler (myPandocCompiler)
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
            { preamble =
                preamble
                  <> "\\usepackage{color}\\usepackage{everysel}"
                  <> "\\EverySelectfont{\\color{red}}"
                  <> "\\everymath{\\color{white}}"
                  <> "\\everydisplay{\\color{white}}"
            }

rules :: RenderFormulae -> Rules ()
rules renderFormulae = do
  match [i|#{postsP}/**.md|] $ do
    route $ setExtension "html"
    compile $
      pandocCompilerWithTransformM
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        (renderFormulae pandocFomulaOptions)
        >>= loadAndApplyTemplate Templates.postT postCtx
        >>= loadAndApplyTemplate Templates.defaultT postCtx
        >>= relativizeUrls

  match [i|#{postsP}/**|] $ do
    route idRoute
    compile copyFileCompiler
