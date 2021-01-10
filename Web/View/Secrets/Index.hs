module Web.View.Secrets.Index where
import Web.View.Prelude
import Web.JsonTypes ( secretToJSON )

data IndexView = IndexView { secrets :: [Secret] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={SecretsAction}>Secrets</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewSecretAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Secret</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach secrets renderSecret}</tbody>
            </table>
        </div>
    |]

    json IndexView {..} = toJSON (secrets |> map secretToJSON)

renderSecret secret = [hsx|
    <tr>
        <td>{secret}</td>
        <td><a href={ShowSecretAction (get #id secret)}>Show</a></td>
        <td><a href={DeleteSecretAction (get #id secret)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
