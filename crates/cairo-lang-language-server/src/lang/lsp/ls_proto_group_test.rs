use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileKind, FileLongId, VirtualFile};
use cairo_lang_filesystem::test_utils::FilesDatabaseForTesting;
use tower_lsp::lsp_types::Url;

use super::LsProtoGroup;

#[test]
fn file_url() {
    let db = FilesDatabaseForTesting::default();

    let check = |expected_url: &str, expected_file_long: FileLongId| {
        let expected_url = Url::parse(expected_url).unwrap();
        let expected_file = db.intern_file(expected_file_long);

        assert_eq!(db.file_for_url(&expected_url), Some(expected_file));
        assert_eq!(db.url_for_file(expected_file), expected_url);
    };

    check("file:///foo/bar", FileLongId::OnDisk("/foo/bar".into()));
    check("file:///", FileLongId::OnDisk("/".into()));

    // NOTE: We expect that Salsa is assigning sequential numeric ids to files,
    //   hence numbers 2 and 3 appear further down.
    check(
        "vfs://2/foo.cairo",
        FileLongId::Virtual(VirtualFile {
            parent: None,
            name: "foo".into(),
            content: Default::default(),
            code_mappings: [].into(),
            kind: FileKind::Module,
        }),
    );
    check(
        "vfs://3/foo%2Fbar.cairo",
        FileLongId::Virtual(VirtualFile {
            parent: None,
            name: "foo/bar".into(),
            content: Default::default(),
            code_mappings: [].into(),
            kind: FileKind::Module,
        }),
    );
}
