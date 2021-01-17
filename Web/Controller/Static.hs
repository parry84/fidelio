module Web.Controller.Static where
import Web.Controller.Prelude
import Web.View.Static.Creator
import Web.View.Static.About

instance Controller StaticController where
    action CreatorAction = render CreatorView
    action AboutAction = render AboutView
