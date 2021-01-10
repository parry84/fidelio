module Web.Controller.Secrets where

import Web.Controller.Prelude
import Web.View.Secrets.Index
import Web.View.Secrets.New
import Web.View.Secrets.Show

instance Controller SecretsController where
    action SecretsAction = do
        secrets <- query @Secret |> fetch
        render IndexView { .. }

    action NewSecretAction = do
        let secret = newRecord
        render NewView { .. }

    action ShowSecretAction { secretId } = do
        secret <- fetch secretId
        render ShowView { .. }

    action CreateSecretAction = do
        let secret = newRecord @Secret
        secret
            |> buildSecret
            |> ifValid \case
                Left secret -> render NewView { .. } 
                Right secret -> do
                    secret <- secret |> createRecord
                    render ShowView { .. }

    action DeleteSecretAction { secretId } = do
        secret <- fetch secretId
        deleteRecord secret
        setSuccessMessage "Secret deleted"
        redirectTo SecretsAction

buildSecret secret = secret
    |> fill @'["payload"]
