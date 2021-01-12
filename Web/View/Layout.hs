module Web.View.Layout (defaultLayout, Html) where

import Generated.Types
import IHP.Controller.RequestContext as RC
import IHP.Environment
import IHP.ViewPrelude
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes
import Web.Types

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
<body>
    <div class="jumbotron text-center">
        <h1 class="mdc-typography--headline1"><a href="/">🎭 Fidelio</a></h1>
        <cite>That is the password... for admittance. But may I ask, what is the password... for the house?</cite>
    </div>
    {inner}
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
