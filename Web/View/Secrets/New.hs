module Web.View.Secrets.New where
import Web.View.Prelude

data NewView = NewView { secret :: Secret }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={SecretsAction}>Secrets</a></li>
                <li class="breadcrumb-item active">New Secret</li>
            </ol>
        </nav>
        <h1>New Secret</h1>
        {renderForm secret}
    |]

renderForm :: Secret -> Html
renderForm secret = formFor secret [hsx|
    {(textField #payload)}
    {submitButton}
|]
