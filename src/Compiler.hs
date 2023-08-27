-- From: <https://laurentrdc.xyz/posts/making-this-website.html>

module Compiler (myPandocCompiler) where

import Hakyll -- hiding (pandocCompiler)
-- import Text.Pandoc
-- import Text.Pandoc.Highlighting

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = Hakyll.pandocCompiler

-- myPandocCompiler :: Compiler (Item String)
-- myPandocCompiler =
--   let mathExtensions =
--         [ Ext_tex_math_dollars,
--           Ext_tex_math_double_backslash,
--           Ext_latex_macros
--         ]
--       codeExtensions =
--         [ Ext_fenced_code_blocks,
--           Ext_fenced_code_attributes,
--           Ext_backtick_code_blocks
--         ]
--       newExtensions = foldr enableExtension defaultExtensions (mathExtensions <> codeExtensions)
--       defaultExtensions = writerExtensions defaultHakyllWriterOptions
--       writerOptions =
--         defaultHakyllWriterOptions
--           { writerExtensions = newExtensions,
--             writerHTMLMathMethod = MathJax "",
--             writerHighlightStyle = Just syntaxHighlightingStyle
--           }
--    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

-- syntaxHighlightingStyle :: Style
-- syntaxHighlightingStyle = haddock
