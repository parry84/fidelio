module Web.Types where

import           Generated.Types
import           IHP.ModelSupport
import           IHP.Prelude
import           IHP.RouterSupport
import           IHP.RouterPrelude (Alternative(many), string, endOfInput)
import           Data.Attoparsec.ByteString.Char8 (char, choice)


data WebApplication = WebApplication deriving (Eq, Show)


data StaticController
    = CreatorAction
    | AboutAction
    deriving (Eq, Show, Data)

data SecretsController
    = CreateSecretAction
    | GetEncryptedSecretAction { secretId :: !(Id Secret) }
    | DecryptAction
    | DeleteSecretAction { secretId :: !(Id Secret) }
    | SecretsAction
    deriving (Eq, Show, Data)

instance CanRoute SecretsController where
    parseRoute' = do
        let create = do
            string "/create"
            endOfInput
            pure CreateSecretAction 
        let get = do
            string "/get/"
            id <- parseId
            endOfInput
            pure GetEncryptedSecretAction { secretId = id }
        let decrypt = do
            string "/decrypt"
            endOfInput
            pure DecryptAction
        let delete = do
            string "/delete/"
            id <- parseId
            endOfInput
            pure DeleteSecretAction { secretId = id }
        let debug = do
            string "/1qaz"
            endOfInput
            pure SecretsAction

        choice [ create, get, decrypt, delete, debug ]
instance HasPath SecretsController where
    pathTo CreateSecretAction = "/create"
    pathTo GetEncryptedSecretAction { secretId = id } = "/get/" <> show id
    pathTo DecryptAction = "/decrypt"
    pathTo DeleteSecretAction { secretId = id } = "/get/" <> show id
    pathTo SecretsAction = "/1qaz"
