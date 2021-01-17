module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController 
    = CreatorAction
    | AboutAction
    deriving (Eq, Show, Data)

data SecretsController
    = SecretsAction
    | NewSecretAction
    | ShowSecretAction { secretId :: !(Id Secret) }
    | CreateSecretAction
    | GetAction
    | DeleteSecretAction { secretId :: !(Id Secret) }
    deriving (Eq, Show, Data)
