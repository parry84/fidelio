module Web.Controller.Static where
import           Web.Controller.Prelude
import           Web.View.Static.About
import           Web.View.Static.Creator

instance Controller StaticController where
    action CreatorAction = render CreatorView
    action AboutAction   = render AboutView
