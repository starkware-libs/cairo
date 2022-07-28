use syntax_codegen::generator::{ensure_file_content, get_codes, project_root};

fn main() {
    for (suffix, code) in get_codes() {
        let filename = project_root().join(suffix);
        ensure_file_content(filename, code);
    }
}
