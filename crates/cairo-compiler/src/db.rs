use std::sync::Arc;

use db_utils::Upcast;
use defs::db::{DefsDatabase, DefsGroup, HasMacroPlugins};
use defs::plugin::MacroPlugin;
use filesystem::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup};
use lowering::db::{init_lowering_group, LoweringDatabase, LoweringGroup};
use parser::db::ParserDatabase;
use plugins::get_default_plugins;
use semantic::db::{SemanticDatabase, SemanticGroup, SemanticGroupEx};
use semantic::plugin::SemanticPlugin;
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
impl RootDatabase {
    pub fn new(extra_plugins: Vec<Arc<dyn SemanticPlugin>>) -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_lowering_group(&mut res);
        // TODO(spapini): Consider taking from config.
        let mut plugins = get_default_plugins();
        plugins.extend(extra_plugins.into_iter());
        res.set_semantic_plugins(plugins);
        res
    }
}
impl Default for RootDatabase {
    fn default() -> Self {
        Self::new(vec![])
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
