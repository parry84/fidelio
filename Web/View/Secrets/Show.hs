module Web.View.Secrets.Show where
import Web.View.Prelude
import Data.Aeson


data ShowView = ShowView { secret :: Secret }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <h1><a href="/">🎭 Fidelio</a></h1>
        <cite>That is the password... for admittance. But may I ask, what is the password... for the house?</cite>
        {secretViewerWidget secret}
    |]

    json ShowView { .. } = toJSON secret
    
instance ToJSON Secret where
    toJSON secret = object
        [ "id" .= get #id secret
        , "payload" .= get #payload secret
        ]

