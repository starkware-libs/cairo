use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin,
    PluginGeneratedFile,
};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use indoc::indoc;

#[derive(Debug, Default)]
pub struct ExposeMacro;
impl NamedPlugin for ExposeMacro {
    const NAME: &'static str = "expose";
}
impl InlineMacroExprPlugin for ExposeMacro {
    fn generate_code<'db>(
        &self,
        db: &'db dyn FilesGroup,
        syntax: &ast::ExprInlineMacro<'db>,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult<'db> {
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
        Some(
            indoc! {r#"
            Injects its contents directly at the call site,
            making all definitions visible in the parent scope.

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
