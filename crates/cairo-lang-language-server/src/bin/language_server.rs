use cairo_lang_language_server::serve_language_service;

#[tokio::main]
async fn main() {
    serve_language_service().await;
}
