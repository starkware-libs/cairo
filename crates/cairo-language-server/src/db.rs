use std::sync::Arc;

use cairo_db_utils::Upcast;
use cairo_defs::db::{DefsDatabase, DefsGroup, HasMacroPlugins};
use cairo_defs::plugin::MacroPlugin;
use cairo_filesystem::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup};
use cairo_lowering::db::{init_lowering_group, LoweringDatabase, LoweringGroup};
use cairo_parser::db::ParserDatabase;
use cairo_plugins::get_default_plugins;
use cairo_semantic::db::{SemanticDatabase, SemanticGroup, SemanticGroupEx};
use cairo_syntax::node::db::{SyntaxDatabase, SyntaxGroup};

#[salsa::database(
    LoweringDatabase,
    SemanticDatabase,
    DefsDatabase,
    ParserDatabase,
    SyntaxDatabase,
    FilesDatabase
)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}
impl salsa::Database for RootDatabase {}
impl Default for RootDatabase {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_lowering_group(&mut res);
        // TODO(spapini): Consider taking from config.
        res.set_semantic_plugins(get_default_plugins());
        res
    }
}
impl AsFilesGroupMut for RootDatabase {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl Upcast<dyn DefsGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn LoweringGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn LoweringGroup + 'static) {
        self
    }
}
impl HasMacroPlugins for RootDatabase {
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>> {
        self.get_macro_plugins()
    }
}
