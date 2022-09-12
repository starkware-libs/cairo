use std::sync::Arc;

use defs::db::DefsGroup;
use defs::ids::ModuleId;
use filesystem::db::{AsFilesGroup, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateLongId, Directory, FileLongId};

use crate::test_utils::SemanticDatabaseForTesting;

fn set_file_content(db: &mut SemanticDatabaseForTesting, path: &str, content: &str) {
    let file_id = db.intern_file(FileLongId::OnDisk(path.into()));
    db.as_files_group_mut().override_file_content(file_id, Some(Arc::new(content.into())));
}

#[test]
fn test_submodules() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;

    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let root = Directory("src".into());
    db.set_crate_root(crate_id, Some(root.clone()));

    // Main module file.
    set_file_content(db, "src/lib.cairo", "mod submod;");
    set_file_content(db, "src/submod.cairo", "mod subsubmod;");
    set_file_content(db, "src/submod/subsubmod.cairo", "func foo() {}");

    // Find submodules.
    let module_id = ModuleId::CrateRoot(crate_id);
    let submodule_id = ModuleId::Submodule(
        *db.module_data(module_id).unwrap().submodules.iter().next().unwrap().0,
    );
    let subsubmodule_id = ModuleId::Submodule(
        *db.module_data(submodule_id).unwrap().submodules.iter().next().unwrap().0,
    );

    db.module_item_by_name(subsubmodule_id, "foo".into())
        .expect("foo() not found in subsubmodule.");
}
