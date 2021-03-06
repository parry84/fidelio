{-# language DeriveAnyClass #-}

module Application.Helper.View (
    -- To use the built in login:
    -- module IHP.LoginSupport.Helper.View
    secretViewerWidget,
    secretCreatorWidget,
    Widget(..),
) where

-- Here you can add functions which are available in all your views
import IHP.ViewPrelude
import Generated.Types
import Data.Aeson as Aeson
import Web.JsonTypes
import qualified Generics.SOP as SOP
import GHC.Generics
import Language.Haskell.To.Elm
    ( HasElmType(elmDefinition),
      HasElmDecoder(elmDecoderDefinition),
      HasElmEncoder(elmEncoderDefinition),
      defaultOptions,
      deriveElmJSONDecoder,
      deriveElmJSONEncoder,
      deriveElmTypeDefinition )

data Widget
  = SecretViewerWidget SecretViewerFlagsJSON
  | SecretCreatorWidget
  deriving ( Generic
           , Aeson.ToJSON
           , SOP.Generic
           , SOP.HasDatatypeInfo
           )

-- haskell-to-elm instances for the Widget type

instance HasElmType Widget where
  elmDefinition =
    Just $ "Api.Generated.Widget"
              |> deriveElmTypeDefinition @Widget
                Language.Haskell.To.Elm.defaultOptions

instance HasElmDecoder Aeson.Value Widget where
  elmDecoderDefinition =
    Just $ "Api.Generated.widgetDecoder"
              |> deriveElmJSONDecoder @Widget
                Language.Haskell.To.Elm.defaultOptions Aeson.defaultOptions

instance HasElmEncoder Aeson.Value Widget where
  elmEncoderDefinition =
    Just $ "Api.Generated.widgetEncoder"
              |> deriveElmJSONEncoder @Widget
                Language.Haskell.To.Elm.defaultOptions Aeson.defaultOptions

-- Widgets

secretViewerWidget :: Text -> Html
secretViewerWidget secretId = [hsx|
    <div data-flags={encode secretData} class="elm"></div>
|]
    where
      secretData :: Widget = SecretViewerWidget $ secretViewerFlagsJSON $ SecretViewerFlags secretId

secretCreatorWidget :: Html
secretCreatorWidget = [hsx|
    <div data-flags={encode SecretCreatorWidget} class="elm"></div>
|]