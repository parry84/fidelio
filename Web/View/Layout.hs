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
<body class="mdc-typography">
    <section>
      <div class="jumbotron text-center">
          <h1 class="mdc-typography--headline1"><a href="/">🎭 Fidelio</a></h1>
          <cite>That is the password... for admittance. But may I ask, what is the password... for the house?</cite>
      </div>
    </section>
    <section>
      {inner}
    </section>
    <section class="jumbotron">
      <h4 class="mdc-typography--headline4">What is it?</h4>
      <p class="mdc-typography--body1">Don't send a password or other form of sensitive information to someone over IM or email. These methods are not secure as anyone with little knowledge can intercept this information during transmission. Using Fidelio you can encrypt your secrets and generate self-destructing links which can be safely transferred to your recipient.</p>
      <h4 class="mdc-typography--headline4">Security features</h4>
      <p class="mdc-typography--body1">Your secret are encrypted client-side on your browser, before being send to the server. Which means it cannot be sniffed by anyone. Decryption is client-side too.
      Moreover, Fidelio is an opensource project and its code can reviewed by everybody at <a href="https://parry84.github.io/fidelio">https://parry84.github.io/fidelio</a>. Pretty secure.</p>
    </section>
    <section>
    <p class="mdc-typography--body2 text-center">Coded by <a href="https://parry84.github.io/">parry84</a> with ❤️</p>
    </section>
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
