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
secretViewerFlagsJSON secretId =
    SecretViewerFlagsJSON {
        secretId = get #secretId secretId
    }

data InputSecret = InputSecret {payload :: Text, password :: Text} deriving (Eq, Show)

data InputSecretJSON = InputSecretJSON
  { payload :: Text
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
    via ElmType "Api.Generated.InputSecret" InputSecretJSON

inputSecretToJSON :: InputSecret -> InputSecretJSON
inputSecretToJSON secret =
    InputSecretJSON {
        payload = get #payload secret,
        password = get #password secret
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
