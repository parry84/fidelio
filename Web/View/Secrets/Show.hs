module Web.View.Secrets.Show where
import Web.View.Prelude
    ( get,
      KeyValue((.=)),
      ToJSON(toJSON),
      Secret,
      Secret'(id, payload),
      hsx,
      View(html, json),
      secretViewerWidget )
import Data.Aeson ( object )


data ShowView = ShowView { secret :: Secret }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {secretViewerWidget secret}
    |]

    json ShowView { .. } = toJSON secret
    
instance ToJSON Secret where
    toJSON secret = object
        [ "id" .= get #id secret
        , "payload" .= get #payload secret
        ]

