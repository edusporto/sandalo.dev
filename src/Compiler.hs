-- From: <https://laurentrdc.xyz/posts/making-this-website.html>

module Compiler (myPandocCompiler, myPandocCompilerWithTransformM, pandocCodeStyle, relativizeUrl, relativizeFuncField) where

import Data.List qualified as L
import Hakyll
import Text.Pandoc
import Text.Pandoc.Highlighting qualified as S

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

-- | Relativize URL. Same logic as "relativizeUrlsWith" in
-- Hakyll.Web.Html.RelativizeUrls, but for just one url.
relativizeUrl ::
  -- | Path to the site root
  String ->
  -- | link to relativize
  String ->
  -- | Resulting link
  String
relativizeUrl root = rel
  where
    isRel :: String -> Bool
    isRel x = "/" `L.isPrefixOf` x && not ("//" `L.isPrefixOf` x)
    rel x = if isRel x then root ++ x else x

-- ugh. ugly name.
relativizeFuncField :: Context a
relativizeFuncField = functionField "relativize" relativize
  where
    relativize :: [String] -> Item a -> Compiler String
    relativize args item = do
      siteRoot <- getRoot <$> getRoute (itemIdentifier item)
      arg <- case args of
        [arg] -> return arg
        _ -> error "relativize: expected only 1 arg"
      return $ relativizeUrl siteRoot arg
    getRoot :: Maybe String -> String
    getRoot = maybe (error "relativize: couldn't get route") toSiteRoot
