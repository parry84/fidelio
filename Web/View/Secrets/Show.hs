module Web.View.Secrets.Show where

import Web.View.Prelude
  ( Text,
    View (html),
    hsx,
    secretViewerWidget
  )

data ShowView = ShowViewOk {secretId :: Text} | ShowViewError {error :: Text}

instance View ShowView where
  html ShowViewOk {..} = [hsx|
        {secretViewerWidget secretId}
    |]

  html ShowViewError {..} = [hsx|
        <div class="container h-100">
            <div class="row h-50 justify-content-center align-items-center">
                <div class="text-center">
                    <h1>404 Secret Not Found</h1>
                    <h2>It either never existed or has already been viewed.</h2>
                </div>
            </div>
        </div>
    |]
