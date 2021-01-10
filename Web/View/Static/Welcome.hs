module Web.View.Static.Welcome where
import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    html WelcomeView = [hsx|
        <h1><a href="/">🎭 Fidelio</a></h1>
        <cite>That is the password... for admittance. But may I ask, what is the password... for the house?</cite>
        {secretCreatorWidget}
    |]