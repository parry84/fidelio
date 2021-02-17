module Web.Controller.Secrets where

import           Web.Controller.Prelude
import           Web.JsonTypes
import           Web.View.Secrets.Index (IndexView (IndexView, secrets))
import           Web.View.Secrets.New   (NewView (NewView, secret))
import           Web.View.Secrets.Show

instance Controller SecretsController where
  action SecretsAction = do
    secrets <- query @Secret |> fetch
    render IndexView {..}
  action NewSecretAction = do
    let secret = newRecord
    render NewView {..}
  action ShowSecretAction {secretId} = do
    maybeSecret <- fetchOneOrNothing secretId
    maybeSecret |> \case
      Just secret -> do
        let expiresAt = get #expiresAt secret
        currentTime <- getCurrentTime
        render $
          if expiresAt > currentTime
            then ShowViewOk $ show $ get #id secret
            else ShowViewError "not_found"
      Nothing -> render $ ShowViewError "not_found"
  --TODO create a type for that error constants

  action GetAction = do
    let id = param @(Id Secret) "id"
    let givenPassword = param @(Text) "password"
    maybeSecret <- fetchOneOrNothing id
    maybeSecret |> \case
      Just secret -> do
        let truePassword = get #password secret
        if truePassword == givenPassword
          then do
            deleteRecord secret
            renderJson $ outputSecretJSON $ buildOutputSecret secret
          else renderPlain "unauthorized"
      Nothing -> renderPlain "not_found"
  action CreateSecretAction = do
    let secret = newRecord @Secret
    let baseUrl' = baseUrl getConfig
    let lifetime = param @(Text) "lifetime"
    secret
      |> buildSecret
      |> ifValid \case
        Left _ -> renderPlain "bad_request"
        Right secret -> do
          secret <- secret |> createRecord
          let now = get #createdAt secret
          let expiresAt = expiration now lifetime
          let id = get #id secret
          let expiringSecret = set #expiresAt expiresAt secret
          expiringSecret |> updateRecord
          let link = baseUrl' ++ "/ShowSecret?secretId=" ++ show id
          renderJson $ linkToJSON $ Link link
  action DeleteSecretAction {secretId} = do
    secret <- fetch secretId
    deleteRecord secret
    setSuccessMessage "Secret deleted"
    redirectTo SecretsAction

instance FromJSON InputPassword where
  parseJSON = withObject "InputPassword" $ \v ->
    InputPassword
      <$> v .: "id"
      <*> v .: "password"

-- TODO convert read to Lifetime type before this switch
expiration :: UTCTime -> Text -> UTCTime
expiration now lifetime = case lifetime of
  "5m"  -> addUTCTime (5 * 60) now
  "10m" -> addUTCTime (10 * 60) now
  "15m" -> addUTCTime (15 * 60) now
  "1h"  -> addUTCTime (3600) now
  "4h"  -> addUTCTime (4 * 3600) now
  "12h" -> addUTCTime (12 * 3600) now
  "1d"  -> addUTCTime (24 * 3600) now
  "3d"  -> addUTCTime (3 * 24 * 3600) now
  "7d"  -> addUTCTime (7 * 24 * 3600) now
  _     -> addUTCTime 0 now

buildSecret secret =
  secret
    |> fill @'["payload"]
    |> fill @'["password"]

buildOutputSecret secret = OutputSecret (get #payload secret)
