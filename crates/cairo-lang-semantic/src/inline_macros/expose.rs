use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin,
    PluginGeneratedFile,
};
use cairo_lang_syntax::node::ast;
use indoc::indoc;
use salsa::Database;

use crate::items::macro_call::{EXPOSE_MACRO_NAME, expose_content_and_mapping};

#[derive(Debug, Default)]
pub struct ExposeMacro;
impl NamedPlugin for ExposeMacro {
    const NAME: &'static str = EXPOSE_MACRO_NAME;
}
impl InlineMacroExprPlugin for ExposeMacro {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        syntax: &ast::ExprInlineMacro<'db>,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult<'db> {
        let Ok((content, code_mapping)) = expose_content_and_mapping(db, syntax.arguments(db))
        else {
            return InlinePluginResult { diagnostics: vec![], ..Default::default() };
        };
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: "expose_inline_macro".into(),
                content,
                code_mappings: vec![code_mapping],
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
