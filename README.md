# To build:

1. Install IHP: https://ihp.digitallyinduced.com/Guide/installation.html
2. install Elm:
```bash
npm install node-elm-compiler parcel-bundler
npm install elm-hot concurrently --save-dev
```

# To launch:

```bash
npm start
```

# To dev:

The application is composed by:
- an IHP MVC container
- an Elm secret generation application: `SecretCreatorWidget`
- an Elm secret decryption application: `SecretViewerWidget`

1. Regenerate Elm types:
```bash
npm gen-types
```
