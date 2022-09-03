use std::sync::Arc;

use filesystem::ids::{CrateLongId, FileLongId, ModuleId, VirtualFile};
use syntax::node::ast::SyntaxFile;

use crate::corelib::core_config;
use crate::db::SemanticGroup;

pub fn setup_test_module(db: &mut dyn SemanticGroup, content: &str) -> (ModuleId, Arc<SyntaxFile>) {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "test.cairo".into(),
        content: Arc::new(content.to_string()),
    }));
    db.set_project_config(core_config(db).with_crate(crate_id, file_id));
    let module_id = ModuleId::CrateRoot(crate_id);
    let module_syntax = db.file_syntax(db.module_file(module_id).unwrap()).expect("").unwrap();
    (module_id, module_syntax)
}
