module Web.Controller.Secrets where

import Web.Controller.Prelude
import Web.View.Secrets.Index ( IndexView(IndexView, secrets) )
import Web.View.Secrets.New ( NewView(NewView, secret) )
import Web.View.Secrets.Show
import Web.JsonTypes


instance Controller SecretsController where
    action SecretsAction = do
        secrets <- query @Secret |> fetch
        render IndexView { .. }

    action NewSecretAction = do
        let secret = newRecord
        render NewView { .. }

    action ShowSecretAction { secretId } = do
        maybeSecret <- fetchOneOrNothing secretId
        maybeSecret |> \case
            Just secret -> render $ ShowViewOk $ show $ get #id secret
            Nothing -> render $ ShowViewError "not_found"

    action GetAction = do
        let id = param @(Id Secret) "id"
        let givenPassword = param @(Text) "password"
        maybeSecret <- fetchOneOrNothing id
        maybeSecret |> \case
            Just secret -> do
                let truePassword = get #password secret
                if truePassword == givenPassword then do
                    deleteRecord secret
                    renderJson $ outputSecretJSON $ buildOutputSecret secret
                else
                    renderPlain "unauthorized"
            Nothing -> renderPlain "not_found"

    action CreateSecretAction = do
        let secret = newRecord @Secret
        let baseUrl' = baseUrl getConfig
        let lifetime = param @(Text) "lifetime"
        secret
            |> buildSecret
            |> ifValid \case
                Left _ -> renderPlain "Error"
                Right secret -> do
                    secret <- secret |> createRecord
                    let now = get #createdAt secret
                    let expiresAt = expiration now lifetime
                    let id = get #id secret
                    let secret2 = set #expiresAt expiresAt secret
                    secret3 <- secret2 |> updateRecord
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

expiration :: UTCTime -> Text -> UTCTime
expiration now lifetime = 
    if lifetime == "Lifetime1d" then
         addUTCTime 100000 now
    else now

buildSecret secret = secret
    |> fill @'["payload"]
    |> fill @'["password"]


buildOutputSecret secret = OutputSecret (get #payload secret)
