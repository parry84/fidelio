module Web.View.Secrets.Edit where
import Web.View.Prelude

data EditView = EditView { secret :: Secret }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={SecretsAction}>Secrets</a></li>
                <li class="breadcrumb-item active">Edit Secret</li>
            </ol>
        </nav>
        <h1>Edit Secret</h1>
        {renderForm secret}
    |]

renderForm :: Secret -> Html
renderForm secret = formFor secret [hsx|
    {(textField #payload)}
    {submitButton}
|]
