use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginGeneratedFile, PluginResult,
};
use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin};
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_parser::db::{ParserGroup, SyntaxNodeExt};
use cairo_lang_syntax::node::{TypedSyntaxNode, ast, both_trivia_width};
use cairo_lang_utils::extract_matches;
use indoc::indoc;
use itertools::Itertools;

use crate::db::SemanticGroup;
use crate::inline_macros::get_default_plugin_suite;
use crate::test_utils::{SemanticDatabaseForTesting, setup_test_module};

#[test]
fn test_resolve() {
    let db_val = SemanticDatabaseForTesting::default();
    let (test_module, _diagnostics) = setup_test_module(
        &db_val,
        indoc! {"
            fn foo() -> felt252 { 5 }
            extern fn felt252_add(a: felt252, b: felt252) -> felt252 nopanic;
        "},
    )
    .split();

    let module_id = test_module.module_id;
    let db = &db_val;
    assert!(db.module_item_by_name(module_id, "doesnt_exist".into()).unwrap().is_none());
    let felt252_add = db.module_item_by_name(module_id, "felt252_add".into()).unwrap();
    assert_eq!(format!("{:?}", felt252_add.debug(db)), "Some(ExternFunctionId(test::felt252_add))");
    match db.module_item_by_name(module_id, "felt252_add".into()).unwrap().unwrap() {
        ModuleItemId::ExternFunction(_) => {}
        _ => panic!("Expected an extern function"),
    };
    match db.module_item_by_name(module_id, "foo".into()).unwrap().unwrap() {
        ModuleItemId::FreeFunction(_) => {}
        _ => panic!("Expected a free function"),
    };
}

#[test]
fn test_resolve_data_full() {
    let db_val = SemanticDatabaseForTesting::default();
    let (test_module, _diagnostics) = setup_test_module(
        &db_val,
        indoc! {"
            trait WithConst<T> { const ITEM: u32; }
            impl ImplWithConst of WithConst<u32> { const ITEM: u32 = 7; }
            const C: u32 = 42;
            fn foo<T, impl I: WithConst<T>>()() {
                let _ = C;
                let _ = I::ITEM;
                let _ = WithConst::<u32>::ITEM;
            }
        "},
    )
    .split();

    let module_id = test_module.module_id;
    let db = &db_val;
    let foo = extract_matches!(
        db.module_item_by_name(module_id, "foo".into()).unwrap().unwrap(),
        ModuleItemId::FreeFunction
    );
    let resolver_data = db.free_function_body_resolver_data(foo).unwrap();
    assert_eq!(resolver_data.resolved_items.generic.len(), 5);
    // Generic item `I` direct usage is the only extra item in the concrete items set.
    assert_eq!(resolver_data.resolved_items.concrete.len(), 6);
}

#[derive(Debug, Default)]
struct MappingsPlugin;
impl MacroPlugin for MappingsPlugin {
    fn generate_code(
        &self,
        db: &dyn ParserGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        // Only run plugin in the test file.
        let ptr = item_ast.stable_ptr(db);
        let file = ptr.0.file_id(db);
        let path = file.full_path(db);
        if path != "lib.cairo" {
            return PluginResult::default();
        }

        // Create code mappings.
        let node = item_ast.as_syntax_node();
        let leaves = node.tokens(db);
        let mut offset = node.span(db).start;
        let code_mappings: Vec<CodeMapping> = leaves
            .into_iter()
            .flat_map(|green| {
                let green_node = db.lookup_intern_green(green);
                let (leading_trivia, trailing_trivia) = both_trivia_width(db, &green_node);
                let leading_trivia = leading_trivia;
                let trailing_trivia = trailing_trivia;

                let width = green_node.width();
                let token_width = width - leading_trivia - trailing_trivia;
                let after_leading_trivia = offset.add_width(leading_trivia);
                let after_token = after_leading_trivia.add_width(token_width);
                let after_trailing_trivia = after_token.add_width(trailing_trivia);

                let prefix = TextSpan { start: offset, end: after_leading_trivia };
                let span_without_trivia =
                    TextSpan { start: after_leading_trivia, end: after_token };
                let suffix = TextSpan { start: after_token, end: after_trailing_trivia };

                offset = offset.add_width(width);

                vec![
                    CodeMapping { span: prefix, origin: CodeOrigin::Span(prefix) },
                    CodeMapping {
                        span: span_without_trivia,
                        origin: CodeOrigin::Span(span_without_trivia),
                    },
                    CodeMapping { span: suffix, origin: CodeOrigin::Span(suffix) },
                ]
            })
            .collect_vec();

        // Return content without changing token sizes.
        let content = indoc! {r#"
            fn main() -> felt252 {
                let x = 51;
                x = 2;
                x
            }
        "#};
        PluginResult {
            code: Some(PluginGeneratedFile {
                name: "virt1".into(),
                content: content.to_string(),
                aux_data: Default::default(),
                diagnostics_note: Default::default(),
                code_mappings,
                is_unhygienic: false,
            }),
            remove_original_item: true,
            ..PluginResult::default()
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        Vec::new()
    }
}

#[test]
fn test_mapping_translate_consecutive_spans() {
    // Setup db with the test plugin.
    let mut suite = get_default_plugin_suite();
    suite.add_plugin::<MappingsPlugin>();
    let mut db_val = SemanticDatabaseForTesting::with_plugin_suite(suite);

    // Create test file.
    let parent_content = indoc! {r#"
            fn main() -> felt252 {
                let x = 42;
                x = 2;
                x
            }
        "#};
    let (test_module, _diagnostics) = setup_test_module(&db_val, parent_content).split();
    let module_id = test_module.module_id;

    // Read semantic diagnostics.
    let db = &mut db_val;
    let diags = db.module_semantic_diagnostics(module_id).unwrap();
    let diags = diags.format(db);
    assert_eq!(
        diags,
        indoc! {r#"
        error: Cannot assign to an immutable variable.
         --> lib.cairo:3:5
            x = 2;
            ^^^^^

    "#}
    );
}
