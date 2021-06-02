{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hakyll.Web.Agda (agdaCompiler) where

import qualified Agda.Interaction.Highlighting.HTML as Agda (generateHTML)
import Agda.Interaction.Options
import qualified Agda.Main as Agda
import Control.Exception (catchJust)
import Control.Monad (void)
import Data.Text (isInfixOf, pack, replace, unpack)
import Hakyll
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.IO.Temp
import Text.Regex.TDFA ((=~))

-- | Compile literate Agda to HTML
agdaCompiler :: Compiler (Item String)
agdaCompiler = cached "Hakyll.Web.Agda.agdaCompilerWith" $ do
  item <- getResourceBody

  let moduleName = agdaModule (itemBody item)
  let inputPath = toFilePath (itemIdentifier item)

  md <- unsafeCompiler $
    withSystemTempDirectory "temp" $ \outputDir -> do
      -- Add input file and HTML options
      let opts =
            defaultOptions
              { optInputFile = Just inputPath,
                optHTMLDir = outputDir,
                optGenerateHTML = True,
                optHTMLHighlight = HighlightAuto
              }

      -- Run Agda
      let tcm = Agda.runAgdaWithOptions [] Agda.generateHTML (Agda.defaultInteraction opts) "agda" opts

      catchJust
        (\case e@ExitSuccess -> Just e; _ -> Nothing)
        (Agda.runTCMPrettyErrors $ void tcm)
        (\_ -> return ())

      -- Read output Markdown file
      readFile $ outputDir </> moduleName <.> "md"

  let fixUrl = \(url :: String) ->
        if pack moduleName `isInfixOf` pack url
          then unpack $ replace (pack $ moduleName <.> "html") "" (pack url)
          else "https://agda.github.io/agda-stdlib/v1.5" </> url
  return $ itemSetBody (withUrls fixUrl md) item

-- | Get Agda module name from code
agdaModule :: String -> String
agdaModule code = case regexResult of
  (_, _, _, [moduleName]) -> moduleName
  _ -> "Main"
  where
    moduleRegex = "module ([^ ]*) where" :: String
    regexResult = code =~ moduleRegex :: (String, String, String, [String])
