# 🎭 Fidelio
>That is the password... for admittance. But may I ask, what is the password... for the house?

## What is it? ##
Fidelio is a web app to generate one-time secrets protected by a passphrase. It is a safer way to share sensitive information rather than using email and IM services. Instead, it allows to generate and send a link to the ecrypted secret and the passphrase to decrypt it (the latter possibly sent over a different channel). Once the secret is read, it is deleted from the server.

## Features ##
In order to achieve a higher security level, the passphrase is never sent to the server. Insted, the following protocol is used:
- The secret is encrypted by the frontend code running inside the browser
- The encrypted secret and an hash of the passphrase is sent to and stored by the server (note that such hash of passphrase cannot be used to decrypt the secret)
- When accessing the link to the secret, the passphrase is asked again
- Then, it is hashed again and sent to the server
- If it match the original hash, the encrypted secret is sent to the frontend and deleted from the server
- The frontend uses the provided passphrase to decrypt and show the secret

## Dependencies ##
* Any recent Linux (we use Debian, Ubuntu, and CentOS)
* Nix (unless you are NixOS)
* Npm
* Elm

## Setup  ##

1. Clone the repository
2. Install Nix and IHP as explained here: https://ihp.digitallyinduced.com/Guide/installation.html
3. install Elm according to the following steps:
```bash
npm install node-elm-compiler parcel-bundler
npm install elm-hot concurrently --save-dev
```
4. launch:

```bash
npm start
```

This project uses `devenv`. To update the environment after changin Nix configuration:
```bash
nix-shell --run 'make -B .envrc'
```

## Architecture ##

The application is composed by following components:
- an IHP MVC container
- an Elm secret generation application: `SecretCreatorWidget`
- an Elm secret decryption application: `SecretViewerWidget`

The Elm types are automatically generated from Haskell types. In order to regenerate them:
```bash
npm gen-types
```

## Similar projects ##
* https://github.com/onetimesecret/onetimesecret