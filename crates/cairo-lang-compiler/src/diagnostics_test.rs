use cairo_lang_filesystem::db::{CrateConfiguration, FilesGroupEx};
use cairo_lang_filesystem::ids::{CrateId, Directory};

use crate::db::RootDatabase;
use crate::diagnostics::get_diagnostics_as_string;

#[test]
fn test_diagnostics() {
    let mut db = RootDatabase::default();

    let crate_id = CrateId::plain(&db, "bad_create");
    db.set_crate_config(
        crate_id,
        Some(CrateConfiguration::default_for_root(Directory::Real("no/such/path".into()))),
    );

    assert_eq!(get_diagnostics_as_string(&db, &[]), "error: no/such/path/lib.cairo not found\n");
}
