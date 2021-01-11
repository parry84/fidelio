module Web.View.Layout (defaultLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Generated.Types
import IHP.Controller.RequestContext as RC
import Web.Types
import Web.Routes

defaultLayout :: Html -> Html
defaultLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>Fidelio</title>
</head>
<body>
    <div class="container mt-4">
        {renderFlashMessages}
        {inner}
    </div>
</body>
|]

stylesheets :: Html
stylesheets = do
    when isDevelopment [hsx|
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link rel="stylesheet" href="/app.css"/>
    |]
    when isProduction [hsx|
        <link rel="stylesheet" href="/prod.css"/>
    |]

scripts :: Html
scripts = do
    when isDevelopment [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="/vendor/flatpickr.js"></script>
        <script src="/helpers.js"></script>
        <script src="/vendor/morphdom-umd.min.js"></script>
        <script defer src="/elm/index.js"></script>
    |]
    when isProduction [hsx|
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
