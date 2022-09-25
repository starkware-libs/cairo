use languageserver::{Backend, RootDatabase, State};
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    #[cfg(feature = "runtime-agnostic")]
    use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    #[cfg(feature = "runtime-agnostic")]
    let (stdin, stdout) = (stdin.compat(), stdout.compat_write());

    let db = RootDatabase::default();
    let (service, socket) = LspService::new(|client| Backend {
        client,
        db_mutex: db.into(),
        state_mutex: State::default().into(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
