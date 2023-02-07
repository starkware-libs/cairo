use cairo_lang_filesystem::db::{FilesGroup, FilesGroupEx};
use cairo_lang_filesystem::ids::{CrateLongId, Directory};

use crate::db::RootDatabase;
use crate::diagnostics::get_diagnostics_as_string;

#[test]
fn test_diagnostics() {
    let mut db = RootDatabase::default();

    let crate_id = db.intern_crate(CrateLongId("bad_create".into()));
    db.set_crate_root(crate_id, Some(Directory("no/such/path".into())));

    assert_eq!(get_diagnostics_as_string(&mut db), "no/such/path/lib.cairo not found\n");
}
