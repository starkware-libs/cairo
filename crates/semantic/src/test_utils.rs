use std::sync::Arc;

use filesystem::db::ProjectConfig;
use filesystem::ids::{CrateLongId, FileLongId, ModuleId, VirtualFile};
use parser::db::ParserGroup;
use syntax::node::ast::SyntaxFile;

pub fn setup_test_module(db: &mut dyn ParserGroup, content: &str) -> (ModuleId, Arc<SyntaxFile>) {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "test.cairo".into(),
        content: Arc::new(content.to_string()),
    }));
    db.set_project_config(ProjectConfig {
        crate_roots: [(crate_id, file_id)].into_iter().collect(),
    });
    let module_id = ModuleId::CrateRoot(crate_id);
    let module_syntax = db.file_syntax(db.module_file(module_id).unwrap()).expect("").unwrap();
    (module_id, module_syntax)
}
