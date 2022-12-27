use std::sync::Arc;

use db_utils::Upcast;
use defs::db::{DefsDatabase, DefsGroup, HasMacroPlugins};
use defs::plugin::MacroPlugin;
use filesystem::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup};
use lowering::db::{init_lowering_group, LoweringDatabase, LoweringGroup};
use parser::db::ParserDatabase;
use plugins::get_default_plugins;
use semantic::db::{Elongate, SemanticDatabase, SemanticGroup, SemanticGroupEx};
use sierra_generator::db::SierraGenDatabase;
use syntax::node::db::{SyntaxDatabase, SyntaxGroup};

#[salsa::database(
    DefsDatabase,
    FilesDatabase,
    LoweringDatabase,
    ParserDatabase,
    SemanticDatabase,
    SierraGenDatabase,
    SyntaxDatabase
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
    fn upcast(&self) -> &(dyn defs::db::DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn semantic::db::SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn LoweringGroup> for RootDatabase {
    fn upcast(&self) -> &(dyn lowering::db::LoweringGroup + 'static) {
        self
    }
}
impl HasMacroPlugins for RootDatabase {
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>> {
        self.get_macro_plugins()
    }
}
impl Elongate for RootDatabase {
    fn elongate(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}
