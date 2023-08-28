-- From: <https://laurentrdc.xyz/posts/making-this-website.html>

module Compiler (myPandocCompiler, myPandocCompilerWithTransformM, pandocCodeStyle) where

import Hakyll
import Text.Pandoc
import qualified Text.Pandoc.Highlighting as S

pandocCodeStyle :: S.Style
pandocCodeStyle = S.breezeDark

myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle = Just pandocCodeStyle
      }

myPandocCompilerWithTransformM ::
  ReaderOptions ->
  WriterOptions ->
  (Pandoc -> Compiler Pandoc) ->
  Compiler (Item String)
myPandocCompilerWithTransformM readerOptions writerOptions =
  pandocCompilerWithTransformM
    readerOptions
    writerOptions {writerHighlightStyle = Just pandocCodeStyle}
