#!/usr/bin/env run-script

module Application.Script.GenerateElmTypes where

import Application.Helper.View
import Application.Script.Prelude
import qualified Data.HashMap.Lazy as HashMap
import Data.Text.IO
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm
import qualified System.Directory as Directory
import Web.JsonTypes

run :: Script
run = do
  let definitions =
        Simplification.simplifyDefinition
          <$> jsonDefinitions @Lifetime
          <> jsonDefinitions @SecretViewerFlagsJSON
          <> jsonDefinitions @SecretJSON
          <> jsonDefinitions @LinkJSON
          <> jsonDefinitions @InputSecretJSON
          <> jsonDefinitions @InputPasswordJSON
          <> jsonDefinitions @OutputSecretJSON
          <> jsonDefinitions @Widget

      modules = Pretty.modules definitions

  Directory.createDirectoryIfMissing False "elm/Api"

  forEach (HashMap.toList modules) $ \(_moduleName, contents) ->
    writeFile "elm/Api/Generated.elm" (show contents)