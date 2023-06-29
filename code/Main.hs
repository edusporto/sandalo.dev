module Main (module Main) where

import Hakyll
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (lift)
import Site.Pages qualified (rules)
import Site.Posts qualified (rules)
import Site.Static qualified (rules)
import Site.Templates qualified (rules)

configuration :: Configuration
configuration =
  defaultConfiguration
    { storeDirectory = ".cache",
      tmpDirectory = ".cache/tmp",
      destinationDirectory = "site",
      providerDirectory = "content",
      previewPort = 8080,
      -- Read deploy command at compile time
      deployCommand = $(runIO (readFile "./deploy.sh") >>= lift)
    }

main :: IO ()
main = hakyllWith configuration $ do
  Site.Static.rules
  Site.Pages.rules
  Site.Posts.rules
  Site.Templates.rules
