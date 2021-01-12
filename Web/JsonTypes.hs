{-# language DeriveAnyClass #-}

module Web.JsonTypes where

import Generated.Types
import IHP.ControllerPrelude
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm
import Application.Lib.DerivingViaElm ( ElmType(..) )

-- JSON serializable types and functions
-- for exposing IHP data to Elm and JSON responses

data SecretJSON = SecretJSON
  { id :: Text
  , payload :: Text
  } deriving ( Generic
             , SOP.Generic
             , SOP.HasDatatypeInfo
             )
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value)
    via ElmType "Api.Generated.Secret" SecretJSON

secretToJSON :: Secret -> SecretJSON
secretToJSON secret =
    SecretJSON {
        id = show $ get #id secret,
        payload = get #payload secret
    }

data LinkJSON = LinkJSON
  { link :: Text
  } deriving ( Generic
             , SOP.Generic
             , SOP.HasDatatypeInfo
             )
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value)
    via ElmType "Api.Generated.Link" LinkJSON

linkToJSON :: Text -> LinkJSON
linkToJSON link =
    LinkJSON {
        link = link
    }