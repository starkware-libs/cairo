use cairo_lang_defs::{
    patcher::{PatchBuilder, RewriteNode},
    plugin::{
        InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin,
        PluginGeneratedFile,
    },
};
use cairo_lang_syntax::node::{
    ast::ExprInlineMacro, db::SyntaxGroup, helpers::WrappedArgListHelper,
};
use indoc::indoc;

#[derive(Debug, Default)]
pub struct MatchesMacro;
impl NamedPlugin for MatchesMacro {
    const NAME: &'static str = "matches";
}
impl InlineMacroExprPlugin for MatchesMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ExprInlineMacro,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        let arguments = syntax.arguments(db).arg_list(db).unwrap().elements(db);
        let mut builder = PatchBuilder::new(db, syntax);
        builder.add_modified(RewriteNode::interpolate_patched(
            r#"
             {
               match $expression$ {
                   $pattern$ => true,
                   _ => false,
               }
             }
            "#,
            &[
                (
                    "expression".to_string(),
                    RewriteNode::from_ast_trimmed(arguments.first().unwrap()),
                ),
                ("pattern".to_string(), RewriteNode::from_ast_trimmed(arguments.get(1).unwrap())),
            ]
            .into(),
        ));
        let (content, code_mappings) = builder.build();
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: format!("{}_macro", Self::NAME).into(),
                content,
                code_mappings,
                aux_data: None,
                diagnostics_note: Default::default(),
            }),
            diagnostics: vec![],
        }
    }

    fn documentation(&self) -> Option<String> {
        Some(
            indoc! {r#"
            Returns whether the given expression matches the provided pattern.
            The pattern syntax is exactly the same as found in a match arm.

            # Syntax
            ```cairo
            let a = Some(5);
            let is_a_some = matches!(a, Some(_));
            ```
            # Returns
            A boolean value indicating whether the expression matches the pattern.

            # Examples
            ```cairo
            let a = Some(1);
            if !matches!(a, Some(_)) {
                panic!("Expected a to be Option::Some, not Option::None");
            }
            ```

            ```cairo
            enum TestEnumWithData {
                VariantA: u32,
                VariantB: u32,
            }

            fn main() {
                let variable = TestEnumWithData::VariantA(5);
                let is_variable_variant_a = matches!(variable, TestEnumWithData::VariantA(_));
                assert!(is_variable_variant_a);
            }
            ```

            ```cairo
            }
            ```
            "#}
            .to_string(),
        )
    }
}
