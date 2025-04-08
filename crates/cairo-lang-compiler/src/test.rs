use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginResult};
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_semantic::test_utils::setup_test_crate;
use cairo_lang_syntax::node::ast::ModuleItem;
use cairo_lang_syntax::node::db::SyntaxGroup;
use indoc::indoc;
use smol_str::SmolStr;

use crate::db::RootDatabase;
use crate::{CompilerConfig, compile_prepared_db_program_artifact};

#[derive(Debug, Default)]
pub struct MockExecutablePlugin {}

impl MacroPlugin for MockExecutablePlugin {
    fn generate_code(
        &self,
        _db: &dyn SyntaxGroup,
        _item_ast: ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        PluginResult { code: None, diagnostics: vec![], remove_original_item: false }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec!["some".to_string()]
    }

    fn executable_attributes(&self) -> Vec<String> {
        vec!["some".to_string()]
    }
}

#[test]
fn can_collect_executables() {
    let content = indoc! {r#"
        fn x() -> felt252 { 12 }

        fn main() -> felt252 { x() }

        #[some]
        fn test() -> felt252 { x() }
    "#};
    let mut suite = PluginSuite::default();
    suite.add_plugin::<MockExecutablePlugin>();
    let mut db =
        RootDatabase::builder().detect_corelib().with_default_plugin_suite(suite).build().unwrap();
    let crate_id = setup_test_crate(&db, content);
    let config = CompilerConfig { replace_ids: true, ..CompilerConfig::default() };
    let artifact = compile_prepared_db_program_artifact(&mut db, vec![crate_id], config).unwrap();
    let executables = artifact.debug_info.unwrap().executables;
    assert!(!executables.is_empty());
    let f_ids = executables.get("some").unwrap();
    assert_eq!(f_ids.len(), 1);
    // Assert executable name
    assert_eq!(f_ids[0].debug_name, Some(SmolStr::new("test::test")));
    // Assert only executable functions are compiled.
    assert_eq!(artifact.program.funcs.len(), 1);
    assert_eq!(artifact.program.funcs[0].id.debug_name, Some(SmolStr::new("test::test")));
}
