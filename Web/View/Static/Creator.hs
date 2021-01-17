module Web.View.Static.Creator where

import Web.View.Prelude ( hsx, View(html), secretCreatorWidget )

data CreatorView = CreatorView

instance View CreatorView where
  html CreatorView =
    [hsx|
        {secretCreatorWidget}
    |]