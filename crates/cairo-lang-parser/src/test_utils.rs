use std::fs;
use std::sync::Arc;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileLongId, VirtualFile};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use smol_str::SmolStr;

use crate::utils::{get_syntax_root_and_diagnostics, SimpleParserDatabase};

pub fn read_file(filename: &str) -> String {
    fs::read_to_string(filename)
        .unwrap_or_else(|_| panic!("Something went wrong reading file {}", filename))
}

pub fn get_diagnostics(inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
    let db = &SimpleParserDatabase::default();
    let code = &inputs["cairo_code"];

    let file_id = create_virtual_file(db, "dummy_file.cairo", code);
    let (_, diagnostics) = get_syntax_root_and_diagnostics(db, file_id, code);
    OrderedHashMap::from([("expected_diagnostics".into(), diagnostics.format(db))])
}

// TODO(yuval): stop virtual files for tests anymore. See semantic tests.
/// Creates a virtual file with the given content and returns its ID.
pub fn create_virtual_file(
    db: &SimpleParserDatabase,
    file_name: impl Into<SmolStr>,
    content: &str,
) -> FileId {
    db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: file_name.into(),
        content: Arc::new(content.into()),
    }))
}
