{-# language DeriveAnyClass #-}

module Web.JsonTypes where

import Generated.Types
import IHP.ControllerPrelude
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm as LHTE
import Application.Lib.DerivingViaElm ( ElmType(..) )

-- JSON serializable types and functions
-- for exposing IHP data to Elm and JSON responses
data Lifetime
    = Lifetime5m
    | Lifetime10m
    | Lifetime15m
    | Lifetime1h
    | Lifetime4h
    | Lifetime12h
    | Lifetime1d
    | Lifetime3d
    | Lifetime7d
    deriving ( Eq, Show, Generic
             , SOP.Generic
             , SOP.HasDatatypeInfo
             )

instance FromJSON Lifetime where
    parseJSON = genericParseJSON Aeson.defaultOptions { constructorTagModifier = lifetimeJ }

instance ToJSON Lifetime where
    toJSON = genericToJSON Aeson.defaultOptions { constructorTagModifier = lifetimeJ }

instance HasElmType Lifetime where
  elmDefinition =
    Just $ deriveElmTypeDefinition @Lifetime LHTE.defaultOptions "Api.Generated.Lifetime"
instance HasElmEncoder Aeson.Value Lifetime where
    elmEncoderDefinition =
        --Just $ deriveElmJSONEncoder @Lifetime (LHTE.Options lifetimeJ) Aeson.defaultOptions { constructorTagModifier = lifetimeJ } "Api.Generated.Lifetime.encoder"
        Just $ deriveElmJSONEncoder @Lifetime LHTE.defaultOptions Aeson.defaultOptions "Api.Generated.Lifetime.encoder"

instance HasElmDecoder Aeson.Value Lifetime where
    elmDecoderDefinition =
        --Just $ deriveElmJSONDecoder @Lifetime (LHTE.Options lifetimeJ) Aeson.defaultOptions { constructorTagModifier = lifetimeJ } "Api.Generated.Lifetime.encoder"
        Just $ deriveElmJSONDecoder @Lifetime LHTE.defaultOptions Aeson.defaultOptions "Api.Generated.Lifetime.decoder"

lifetimeJ :: String -> String
lifetimeJ "Lifetime5m"  = "5m"
lifetimeJ "Lifetime10m" = "10m"
lifetimeJ "Lifetime15m" = "15m"
lifetimeJ "Lifetime1h"  = "1h"
lifetimeJ "Lifetime4h"  = "4h"
lifetimeJ "Lifetime12h" = "12h"
lifetimeJ "Lifetime1d"  = "1d"
lifetimeJ "Lifetime3d"  = "3d"
lifetimeJ "Lifetime7d"  = "7d"
lifetimeJ s = s

data SecretJSON = SecretJSON
  { id :: Text
  , payload :: Text
  , password :: Text
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
        payload = get #payload secret,
        password = get #password secret
    }
    
data Link = Link {link :: Text} deriving (Eq, Show)

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

linkToJSON :: Link -> LinkJSON
linkToJSON link =
    LinkJSON {
        link = get #link link
    }

data SecretViewerFlags = SecretViewerFlags {secretId :: Text} deriving (Eq, Show)

data SecretViewerFlagsJSON = SecretViewerFlagsJSON
  { secretId :: Text
  } deriving ( Generic
             , SOP.Generic
             , SOP.HasDatatypeInfo
             )
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value)
    via ElmType "Api.Generated.SecretViewerFlags" SecretViewerFlagsJSON

secretViewerFlagsJSON :: SecretViewerFlags -> SecretViewerFlagsJSON
secretViewerFlagsJSON secretViewerFlags =
    SecretViewerFlagsJSON {
        secretId = get #secretId secretViewerFlags
    }

data InputSecret = InputSecret {payload :: Text, password :: Text, lifetime :: Lifetime} deriving (Eq, Show)

data InputSecretJSON = InputSecretJSON
  { payload :: Text
  , password :: Text
  , lifetime :: Lifetime
  } deriving ( Generic
             , SOP.Generic
             , SOP.HasDatatypeInfo
             )
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value)
    via ElmType "Api.Generated.InputSecret" InputSecretJSON

inputSecretToJSON :: InputSecret -> InputSecretJSON
inputSecretToJSON secret =
    InputSecretJSON {
        payload = get #payload secret,
        password = get #password secret,
        lifetime = get #lifetime secret
    }

data InputPassword = InputPassword {id :: Text, password :: Text} deriving (Eq, Show)

data InputPasswordJSON = InputPasswordJSON
  { id :: Text
  , password :: Text
  } deriving ( Generic
             , SOP.Generic
             , SOP.HasDatatypeInfo
             )
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value)
    via ElmType "Api.Generated.InputPassword" InputPasswordJSON

inputPasswordJSON :: InputPassword -> InputPasswordJSON
inputPasswordJSON password =
    InputPasswordJSON {
        id = get #id password,
        password = get #password password
    }

data OutputSecret = OutputSecret {payload :: Text} deriving (Eq, Show)

data OutputSecretJSON = OutputSecretJSON
  { payload :: Text
  } deriving ( Generic
             , SOP.Generic
             , SOP.HasDatatypeInfo
             )
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value)
    via ElmType "Api.Generated.OutputSecret" OutputSecretJSON

outputSecretJSON :: OutputSecret -> OutputSecretJSON
outputSecretJSON secret =
    OutputSecretJSON {
        payload = get #payload secret
    }
