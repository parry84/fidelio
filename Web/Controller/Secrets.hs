module Web.Controller.Secrets where

import           Control.Concurrent
import           Web.Controller.Prelude
import           Web.JsonTypes
import           Web.View.Secrets.Index (IndexView (IndexView, secrets))
import           Web.View.Secrets.New   (NewView (NewView, secret))
import           Web.View.Secrets.Show


instance Controller SecretsController where
  action SecretsAction = do
    secrets <- query @Secret |> fetch
    render IndexView {..}

  action GetEncryptedSecretAction {secretId} = do
    maybeSecret <- fetchOneOrNothing secretId
    maybeSecret |> \case
      Just secret -> do
        let expiresAt = get #expiresAt secret
        let currentViews = get #viewCount secret
        let maxViews = 1
        currentTime <- getCurrentTime
        render $
          if expiresAt > currentTime && currentViews < maxViews
            then
              ShowViewOk $ show $ get #id secret
            else
              ShowViewError "not_found"
      Nothing -> render $ ShowViewError "not_found"
  --TODO create a type for that error constants

  action DecryptAction = do
    let id = param @(Id Secret) "id"
    let givenPassword = param @Text "password"
    maybeSecret <- fetchOneOrNothing id
    maybeSecret |> \case
      Just secret -> do
        let truePassword = get #password secret
        if truePassword == givenPassword
          then do
            handleMaxViews secret
            renderJson $ outputSecretJSON $ buildOutputSecret secret
          else do
            modify #failedAttemptsCount (+1) secret |> updateRecord
            renderPlain "unauthorized"
      Nothing -> renderPlain "not_found"

  action CreateSecretAction = do
    purgeExpiredSecrets
    let secret = newRecord @Secret
    let lifetime = param @Text "lifetime"
    let payloadType = param @PayloadType "payload_type"
    secret
      |> buildSecret
      |> ifValid \case
        Left _ -> renderPlain "bad_request"
        Right secret -> do
          secret <- secret |> createRecord
          setLifetime secret lifetime
          setPayloadType secret payloadType
          let link = baseUrl getConfig ++ "/get/" ++ show (get #id secret)
          renderJson $ linkToJSON $ Link link

  action DeleteSecretAction {secretId} = do
    secret <- fetch secretId
    deleteRecord secret
    setSuccessMessage "Secret deleted"
    redirectTo SecretsAction

handleMaxViews :: (?modelContext::ModelContext) => Secret -> IO ()
handleMaxViews secret = do
  let maxViews = 1
  let secret' = modify #viewCount (+1) secret
  if get #viewCount secret' >= maxViews
    then
      deleteRecord secret'
    else do
      secret |> updateRecord
      pure ()

purgeExpiredSecrets :: (?modelContext::ModelContext) => IO ()
purgeExpiredSecrets = do
  expiredSecrets <- query @Secret
    |> filterWhereSql (#expiresAt, "> now()")
    |> fetch
  deleteRecords expiredSecrets

setLifetime :: (?modelContext::ModelContext) => Secret -> Text -> IO Secret
setLifetime secret lifetime = do
  now <- getCurrentTime
  let expiresAt = expiration now lifetime
  let secret' = set #expiresAt expiresAt secret
  secret' |> updateRecord

setPayloadType :: (?modelContext::ModelContext) => Secret -> PayloadType  -> IO Secret
setPayloadType secret payloadType = do
  let secret' = set #payloadType payloadType secret
  secret' |> updateRecord

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
  "1h"  -> addUTCTime 3600 now
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

buildOutputSecret :: Secret -> OutputSecret
buildOutputSecret secret = OutputSecret (get #payloadType secret) (get #payload secret)
