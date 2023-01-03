use cairo_lang_syntax_codegen::generator::{ensure_file_content, get_codes, project_root};
use cairo_lang_utils::logging::init_logging;

fn main() {
    init_logging(log::LevelFilter::Info);
    log::info!("Starting syntax generation.");

    for (suffix, code) in get_codes() {
        let filename = project_root().join(suffix);
        ensure_file_content(filename, code);
    }
}
