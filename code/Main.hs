module Main (module Main) where

import Hakyll
import Site.Pages (rules)
import Site.Posts (rules)
import Site.Static (rules)
import Site.Templates (rules)

configuration :: Configuration
configuration =
  defaultConfiguration
    { storeDirectory = ".cache",
      tmpDirectory = ".cache/tmp",
      destinationDirectory = "site",
      providerDirectory = "content",
      previewPort = 8080
    }

main :: IO ()
main = hakyllWith configuration $ do
  Site.Static.rules
  Site.Pages.rules
  Site.Posts.rules
  Site.Templates.rules
