module Web.View.Secrets.Show where
import Web.View.Prelude
    ( Secret,
      hsx,
      View(html),
      secretViewerWidget )

data ShowView = ShowView { secret :: Secret }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {secretViewerWidget secret}
    |]
