use std::sync::Arc;

use cairo_lang_defs::db::{DefsDatabase, DefsGroup, HasMacroPlugins};
use cairo_lang_defs::plugin::MacroPlugin;
use cairo_lang_filesystem::db::{init_files_group, AsFilesGroupMut, FilesDatabase, FilesGroup};
use cairo_lang_parser::db::ParserDatabase;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_semantic::db::{SemanticDatabase, SemanticGroup, SemanticGroupEx};
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_utils::Upcast;

use crate::db::{init_lowering_group, LoweringDatabase, LoweringGroup};

#[salsa::database(
    LoweringDatabase,
    SemanticDatabase,
    DefsDatabase,
    ParserDatabase,
    SyntaxDatabase,
    FilesDatabase
)]
pub struct LoweringDatabaseForTesting {
    storage: salsa::Storage<LoweringDatabaseForTesting>,
}
impl salsa::Database for LoweringDatabaseForTesting {}
impl Default for LoweringDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        init_lowering_group(&mut res);
        res.set_semantic_plugins(get_default_plugins());
        res
    }
}
impl AsFilesGroupMut for LoweringDatabaseForTesting {
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl Upcast<dyn SyntaxGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl Upcast<dyn DefsGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}
impl Upcast<dyn SemanticGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn SemanticGroup + 'static) {
        self
    }
}
impl Upcast<dyn LoweringGroup> for LoweringDatabaseForTesting {
    fn upcast(&self) -> &(dyn LoweringGroup + 'static) {
        self
    }
}
impl HasMacroPlugins for LoweringDatabaseForTesting {
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>> {
        self.get_macro_plugins()
    }
}
