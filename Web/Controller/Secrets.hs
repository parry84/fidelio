module Web.Controller.Secrets where

import Web.Controller.Prelude
import Web.View.Secrets.Index ( IndexView(IndexView, secrets) )
import Web.View.Secrets.New ( NewView(NewView, secret) )
import Web.View.Secrets.Show ( ShowView(ShowView, secret) )
import Web.JsonTypes


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

    action GetAction = do
        let id = param @(Id Secret) "id"
        let givenPassword = param @(Text) "password"
        secret <- fetch id
        let truePassword = get #password secret
        if truePassword == givenPassword then
            renderJson $ outputSecretJSON $ buildOutputSecret secret
        else
            renderPlain "Unauthorized"

    action CreateSecretAction = do
        let secret = newRecord @Secret
        let baseUrl' = baseUrl getConfig
        secret
            |> buildSecret
            |> ifValid \case
                Left _ -> renderPlain "Error"
                Right secret -> do
                    secret <- secret |> createRecord
                    let id = get #id secret
                    let link = baseUrl' ++ "/ShowSecret?secretId=" ++ show id
                    renderJson $ linkToJSON $ Link link

    action DeleteSecretAction { secretId } = do
        secret <- fetch secretId
        deleteRecord secret
        setSuccessMessage "Secret deleted"
        redirectTo SecretsAction

instance FromJSON InputPassword where
    parseJSON = withObject "InputPassword" $ \v -> InputPassword
        <$> v .: "id"
        <*> v .: "password"

buildSecret secret = secret
    |> fill @'["payload"]
    |> fill @'["password"]

buildOutputSecret secret = OutputSecret (get #payload secret)
