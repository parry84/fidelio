module Web.View.Secrets.Show where
import Web.View.Prelude

data ShowView = ShowView { secret :: Secret }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={SecretsAction}>Secrets</a></li>
                <li class="breadcrumb-item active">Show Secret</li>
            </ol>
        </nav>
        <h1>Show Secret</h1>
        <p>{secret}</p>
    |]
