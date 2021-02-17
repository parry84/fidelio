module Web.View.Static.Creator where

import           Web.View.Prelude (View (html), hsx, secretCreatorWidget)

data CreatorView = CreatorView

instance View CreatorView where
  html CreatorView =
    [hsx|
        {secretCreatorWidget}
    |]
