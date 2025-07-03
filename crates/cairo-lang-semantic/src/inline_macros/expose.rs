use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin,
    PluginGeneratedFile,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};

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
        let tokens = match syntax.arguments(db).subtree(db) {
            ast::WrappedTokenTree::Braced(braced) => braced.tokens(db),
            ast::WrappedTokenTree::Parenthesized(paren) => paren.tokens(db),
            ast::WrappedTokenTree::Bracketed(bracket) => bracket.tokens(db),
            ast::WrappedTokenTree::Missing(_) => {
                return InlinePluginResult { diagnostics: vec![], ..Default::default() };
            }
        };
        let mut builder = PatchBuilder::new(db, syntax);
        builder.add_node(tokens.as_syntax_node());
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
        Some("Makes all definitions in its contents visible in the parent scope.".to_string())
    }
}
