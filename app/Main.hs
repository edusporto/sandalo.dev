module Main (module Main) where

import Hakyll
import Hakyll.Contrib.LaTeX (initFormulaCompilerDataURI)
import Image.LaTeX.Render (defaultEnv)
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (lift)
import Site.Pages qualified (rules)
import Site.Posts qualified (rules)
import Site.Projects qualified (rules)
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
      deployCommand = $(runIO (readFile "./deploy/deploy.sh") >>= lift)
    }

main :: IO ()
main = do
  renderFormulae <- initFormulaCompilerDataURI 1000 defaultEnv
  hakyllWith configuration $ do
    Site.Static.rules
    Site.Pages.rules
    Site.Posts.rules renderFormulae
    Site.Projects.rules
    Site.Templates.rules
