module Web.View.Layout (defaultLayout, Html) where

import           Generated.Types
import           IHP.Controller.RequestContext as RC
import           IHP.Environment
import           IHP.ViewPrelude
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Web.Routes
import           Web.Types

defaultLayout :: Html -> Html
defaultLayout inner =
  H.docTypeHtml ! A.lang "en" $
    [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>Fidelio</title>
</head>
<body class="mdc-typography">
  <div>
    <header>
      <div class="jumbotron text-center">
        <h1 class="mdc-typography--headline1"><a href="/">🎭 Fidelio</a></h1>
        <cite>That is the password... for admittance. But may I ask, what is the password... for the house?</cite>
      </div>
    </header>
    <main>
      {inner}
    </main>
    <footer class="fixed-bottom">
      <nav class="navbar navbar-expand-lg navbar-light bg-light">
        <div class="collapse navbar-collapse" id="navbarSupportedContent">
          <ul class="navbar-nav mr-auto">
            <li class="nav-item">
              <a class="nav-link" href="/">Home</a>
            </li>
            <li class="nav-item">
              <a class="nav-link" href="/About">About</a>
            </li>
          </ul>

          <ul class="navbar-nav ml-md-auto">
            <li class="nav-item align-middle">
              <div class="navbar-text mdc-typography--body2  mr-0 mr-md-2" style="margin-top: 15px;">Coded by <a href="https://parry84.github.io/">parry84</a> with ❤️</div>
            </li>
            <li class="nav-item">
              <a class="nav-link mr-0 mr-md-2" href="https://ihpcloud.com/NewUser?referredBy=99167028-d8ad-4d52-841a-92a81f31f81b">
                <img class="navbar-brand" src="https://ihpcloud.com/deployed-with-ihp-cloud-blue.svg" style="width:100px; margin-top: -0px;" alt="Deployed with IHP Cloud"/>
              </a>
            </li>
          </ul>

        </div>
      </nav>
    </footer>
  </div>
</body>
|]

stylesheets :: Html
stylesheets = do
  when
    isDevelopment
    [hsx|
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link href="https://fonts.googleapis.com/css?family=Roboto:300,400,500|Material+Icons" rel="stylesheet">
        <link rel="stylesheet" href="https://unpkg.com/material-components-web-elm@6.0.0/dist/material-components-web-elm.min.css">
        <link rel="stylesheet" href="/app.css"/>
    |]
  when
    isProduction
    [hsx|
        <link href="https://fonts.googleapis.com/css?family=Roboto:300,400,500|Material+Icons" rel="stylesheet">
        <link rel="stylesheet" href="https://unpkg.com/material-components-web-elm@6.0.0/dist/material-components-web-elm.min.css">
        <link rel="stylesheet" href="/prod.css"/>
    |]

scripts :: Html
scripts = do
  when
    isDevelopment
    [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="/vendor/flatpickr.js"></script>
        <script src="/helpers.js"></script>
        <script src="/vendor/morphdom-umd.min.js"></script>
        <script src="https://unpkg.com/material-components-web-elm@6.0.0/dist/material-components-web-elm.min.js"></script>
        <script defer src="/elm/index.js"></script>
    |]
  when
    isProduction
    [hsx|
        <script src="https://unpkg.com/material-components-web-elm@6.0.0/dist/material-components-web-elm.min.js"></script>
        <script defer src="/prod.js"></script>
    |]

metaTags :: Html
metaTags = do
  let hostname = appHostname getConfig
  [hsx|
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
        <meta property="og:title" content="Fidelio"/>
        <meta property="og:type" content="website"/>
        <meta property="og:url" content={hostname}/>
        <meta property="og:description" content="A trustable secret sharing app"/>
    |]
