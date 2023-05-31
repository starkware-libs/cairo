use cairo_lang_language_server::serve_language_service;
use cairo_lang_utils::logging::init_logging;

#[tokio::main]
async fn main() {
    init_logging(log::LevelFilter::Warn);
    serve_language_service().await;
}
