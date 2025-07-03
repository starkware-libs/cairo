use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin,
    PluginGeneratedFile,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use indoc::indoc;

#[derive(Debug, Default)]
pub struct ExposeMacro;
impl NamedPlugin for ExposeMacro {
    const NAME: &'static str = "expose";
}
impl InlineMacroExprPlugin for ExposeMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        let arguments = syntax.arguments(db);
        let mut builder = PatchBuilder::new(db, syntax);
        let token_tree_node = ast::TokenTreeNode::from_syntax_node(db, arguments.as_syntax_node());
        let subtree = token_tree_node.subtree(db);

        match subtree {
            ast::WrappedTokenTree::Braced(braced) => {
                for token in braced.tokens(db).elements(db) {
                    builder.add_node(token.as_syntax_node());
                }
            }
            ast::WrappedTokenTree::Parenthesized(paren) => {
                for token in paren.tokens(db).elements(db) {
                    builder.add_node(token.as_syntax_node());
                }
            }
            ast::WrappedTokenTree::Bracketed(bracket) => {
                for token in bracket.tokens(db).elements(db) {
                    builder.add_node(token.as_syntax_node());
                }
            }
            _ => {
                builder.add_node(arguments.as_syntax_node());
            }
        }
        let (content, code_mappings) = builder.build();
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: "expose_inline_macro".into(),
                content,
                code_mappings,
                aux_data: None,
                diagnostics_note: Default::default(),
                is_unhygienic: true,
            }),
            diagnostics: vec![],
        }
    }

    fn documentation(&self) -> Option<String> {
        Some(
            indoc! {r#"
            Injects its contents directly at the call site, making all definitions visible in the parent scope.

            # Syntax
            ```cairo
            expose!{ let a = 42; let b = a + 1; }
            expose!( let a = 42; let b = a + 1; )
            expose![ let a = 42; let b = a + 1; ]
            ```
            "#}
            .to_string(),
        )
    }
}
