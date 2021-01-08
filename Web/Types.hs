module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data SecretsController
    = SecretsAction
    | NewSecretAction
    | ShowSecretAction { secretId :: !(Id Secret) }
    | CreateSecretAction
    | EditSecretAction { secretId :: !(Id Secret) }
    | UpdateSecretAction { secretId :: !(Id Secret) }
    | DeleteSecretAction { secretId :: !(Id Secret) }
    deriving (Eq, Show, Data)
