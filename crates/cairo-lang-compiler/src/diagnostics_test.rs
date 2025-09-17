use cairo_lang_filesystem::db::CrateConfiguration;
use cairo_lang_filesystem::ids::{CrateId, Directory, SmolStrId};
use cairo_lang_filesystem::set_crate_config;

use crate::db::RootDatabase;
use crate::diagnostics::get_diagnostics_as_string;

#[test]
fn test_diagnostics() {
    let mut db = RootDatabase::default();
    let db_ref = &mut db;
    let crate_id = CrateId::plain(db_ref, SmolStrId::from(db_ref, "bad_crate"));
    set_crate_config!(
        db_ref,
        crate_id,
        Some(CrateConfiguration::default_for_root(Directory::Real("no/such/path".into())))
    );

    assert_eq!(get_diagnostics_as_string(&db, None), "error: no/such/path/lib.cairo not found\n");
}
