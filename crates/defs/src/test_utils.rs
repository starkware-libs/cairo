use std::sync::Arc;

use filesystem::db::FilesGroupEx;
use filesystem::ids::{CrateLongId, Directory};

use crate::db::DefsGroup;
use crate::ids::ModuleId;

pub fn setup_test_module<T: DefsGroup + ?Sized>(db: &mut T, content: &str) -> ModuleId {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let directory = Directory("src".into());
    db.set_crate_root(crate_id, Some(directory));
    let file = db.module_file(ModuleId::CrateRoot(crate_id)).unwrap();
    db.as_files_group_mut().override_file_content(file, Some(Arc::new(content.to_string())));
    ModuleId::CrateRoot(crate_id)
}
