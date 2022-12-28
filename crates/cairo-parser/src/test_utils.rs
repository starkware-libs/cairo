use std::fs;
use std::sync::Arc;

use filesystem::db::FilesGroup;
use filesystem::ids::{FileId, FileLongId, VirtualFile};
use smol_str::SmolStr;
use utils::ordered_hash_map::OrderedHashMap;

use crate::utils::{get_syntax_root_and_diagnostics, SimpleParserDatabase};

pub fn read_file(filename: &str) -> String {
    fs::read_to_string(filename)
        .unwrap_or_else(|_| panic!("Something went wrong reading file {}", filename))
}

pub fn get_diagnostics(
    db: &mut SimpleParserDatabase,
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
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

#[macro_export]
macro_rules! parser_test {
    ($test_name:ident, $filenames:expr, $func:ident) => {
        test_utils::test_file_test!($test_name, $filenames, SimpleParserDatabase, $func);
    };
}
