module Web.View.Secrets.New where
import           Web.View.Prelude

data NewView = NewView { secret :: Secret }

instance View NewView where
    html NewView { .. } = [hsx|
        <h1><a href="/">🎭 Fidelio</a></h1>
        <cite>That is the password... for admittance. But may I ask, what is the password... for the house?</cite>
        {secretCreatorWidget}
    |]

