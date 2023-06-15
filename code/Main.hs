module Main (module Main) where

import Hakyll
import Site.Pages (rules)
import Site.Posts (rules)
import Site.Static (rules)
import Site.Templates (rules)

main :: IO ()
main = hakyll $ do
  Site.Static.rules
  Site.Pages.rules
  Site.Posts.rules
  Site.Templates.rules
