module Web.View.Static.Welcome where

import Web.View.Prelude ( hsx, View(html), secretCreatorWidget )

data WelcomeView = WelcomeView

instance View WelcomeView where
  html WelcomeView =
    [hsx|
        {secretCreatorWidget}
    |]