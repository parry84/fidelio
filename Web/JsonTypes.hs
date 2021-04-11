{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Web.JsonTypes where

import           Application.Lib.DerivingViaElm (ElmType (..))
import qualified Data.Aeson                     as Aeson
import           Data.Char                      as Char (toLower)
import           GHC.Generics                   (Generic (Rep))
import           Generated.Types
import qualified Generics.SOP                   as SOP
import           IHP.ControllerPrelude
import           Language.Haskell.To.Elm        as LHTE


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
        Just $ deriveElmJSONEncoder @Lifetime (LHTE.Options lifetimeJ) Aeson.defaultOptions { constructorTagModifier = lifetimeJ } "Api.Generated.lifetimeEncoder"

instance HasElmDecoder Aeson.Value Lifetime where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Lifetime (LHTE.Options lifetimeJ) Aeson.defaultOptions { constructorTagModifier = lifetimeJ } "Api.Generated.lifetimeDecoder"

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
lifetimeJ s             = s

deriving instance Generic PayloadType
deriving instance SOP.Generic PayloadType
deriving instance SOP.HasDatatypeInfo PayloadType
instance FromJSON PayloadType where
    parseJSON = genericParseJSON Aeson.defaultOptions

instance ToJSON PayloadType where
    toJSON = genericToJSON Aeson.defaultOptions { constructorTagModifier = map Char.toLower }

instance HasElmType PayloadType where
  elmDefinition =
    Just $ deriveElmTypeDefinition @PayloadType LHTE.defaultOptions "Api.Generated.PayloadType"
instance HasElmEncoder Aeson.Value PayloadType where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @PayloadType (LHTE.Options IHP.ControllerPrelude.id) Aeson.defaultOptions { constructorTagModifier = map Char.toLower } "Api.Generated.payloadTypeEncoder"

instance HasElmDecoder Aeson.Value PayloadType where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @PayloadType (LHTE.Options IHP.ControllerPrelude.id) Aeson.defaultOptions { constructorTagModifier = map Char.toLower } "Api.Generated.payloadTypeDecoder"

data SecretJSON = SecretJSON
  { id          :: Text
  , payloadType :: PayloadType
  , payload     :: Text
  , password    :: Text
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
        payloadType = get #payloadType secret,
        payload = get #payload secret,
        password = get #password secret
    }

newtype Link = Link {link :: Text} deriving (Eq, Show)

newtype LinkJSON = LinkJSON
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

newtype SecretViewerFlags = SecretViewerFlags {secretId :: Text} deriving (Eq, Show)

newtype SecretViewerFlagsJSON = SecretViewerFlagsJSON
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

data InputSecret = InputSecret {payloadType :: PayloadType, payload :: Text, password :: Text, lifetime :: Lifetime} deriving (Eq, Show)

newtype SnakeCaseJson a = SnakeCaseJson a
  deriving (Eq, Show, Generic)

instance (GToJSON Zero (Rep a), Generic a) => ToJSON (SnakeCaseJson a) where
  toJSON (SnakeCaseJson a) = genericToJSON Aeson.defaultOptions { fieldLabelModifier = camelTo2 '_',  constructorTagModifier = camelTo2 '_' } a

data InputSecretJSON = InputSecretJSON
  { payloadType :: PayloadType
  , payload     :: Text
  , password    :: Text
  , lifetime    :: Lifetime
  } deriving ( Generic
             , SOP.Generic
             , SOP.HasDatatypeInfo
             , Aeson.FromJSON
             )
    deriving ( Aeson.ToJSON
             ) via SnakeCaseJson InputSecretJSON

inputSecretToJSON :: InputSecret -> InputSecretJSON

inputSecretToJSON secret =
    InputSecretJSON {
        payloadType = get #payloadType secret,
        payload = get #payload secret,
        password = get #password secret,
        lifetime = get #lifetime secret
    }


instance HasElmType InputSecretJSON where
  elmDefinition =
    Just $ deriveElmTypeDefinition @InputSecretJSON LHTE.defaultOptions "Api.Generated.InputSecret"

instance HasElmEncoder Aeson.Value InputSecretJSON where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @InputSecretJSON (LHTE.Options IHP.ControllerPrelude.id) Aeson.defaultOptions { fieldLabelModifier = camelTo2 '_',  constructorTagModifier = camelTo2 '_' } "Api.Generated.inputSecretEncoder"

instance HasElmDecoder Aeson.Value InputSecretJSON where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @InputSecretJSON (LHTE.Options IHP.ControllerPrelude.id) Aeson.defaultOptions { fieldLabelModifier = camelTo2 '_',  constructorTagModifier = camelTo2 '_' } "Api.Generated.inputSecretDecoder"


data InputPassword = InputPassword {id :: Text, password :: Text} deriving (Eq, Show)

data InputPasswordJSON = InputPasswordJSON
  { id       :: Text
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

data OutputSecret = OutputSecret {payloadType :: PayloadType, payload :: Text} deriving (Eq, Show)

data OutputSecretJSON = OutputSecretJSON
  { payloadType :: PayloadType
  , payload     :: Text
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
        payloadType = get #payloadType secret,
        payload = get #payload secret
    }
