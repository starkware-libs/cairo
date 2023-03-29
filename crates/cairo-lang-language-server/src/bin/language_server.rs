use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_language_server::{Backend, State};
use cairo_lang_starknet::db::StarknetRootDatabaseBuilderEx;
use cairo_lang_utils::logging::init_logging;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    init_logging(log::LevelFilter::Warn);

    #[cfg(feature = "runtime-agnostic")]
    use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    #[cfg(feature = "runtime-agnostic")]
    let (stdin, stdout) = (stdin.compat(), stdout.compat_write());

    let db = RootDatabase::builder()
        .with_cfg(CfgSet::from_iter([Cfg::tag("test")]))
        .detect_corelib()
        .with_starknet()
        .build()
        .expect("Failed to initialize Cairo compiler database.");

    let (service, socket) = LspService::build(|client| Backend {
        client,
        db_mutex: db.into(),
        state_mutex: State::default().into(),
    })
    .custom_method("vfs/provide", Backend::vfs_provide)
    .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
