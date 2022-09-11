use std::collections::HashMap;
use std::sync::Arc;

use filesystem::db::{FilesGroupEx, ProjectConfig};
use filesystem::ids::{CrateLongId, Directory};

use crate::db::DefsGroup;
use crate::ids::ModuleId;

pub fn setup_test_module<T: DefsGroup + ?Sized>(db: &mut T, content: &str) -> (ModuleId, String) {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let directory = Directory("src".into());
    db.set_project_config(ProjectConfig { crate_roots: HashMap::from([(crate_id, directory)]) });
    let file = db.module_file(ModuleId::CrateRoot(crate_id)).unwrap();
    db.as_files_group_mut().override_file_content(file, Some(Arc::new(content.to_string())));
    let syntax_diagnostics = db.file_syntax_diagnostics(file).format(db.as_files_group());
    (ModuleId::CrateRoot(crate_id), syntax_diagnostics)
}
