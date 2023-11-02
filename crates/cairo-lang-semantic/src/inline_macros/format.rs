use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, NamedPlugin, PluginGeneratedFile,
};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;

use super::formatted::FormattedInfo;

/// Macro for formatting.
#[derive(Default, Debug)]
pub struct FormatMacro;
impl NamedPlugin for FormatMacro {
    const NAME: &'static str = "format";
}
impl InlineMacroExprPlugin for FormatMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        const FORMATTER_NAME: &str = "__formatter_for_format_macro_";
        let info = match FormattedInfo::extract(db, syntax, Some(FORMATTER_NAME)) {
            Ok(info) => info,
            Err(diagnostics) => return InlinePluginResult { code: None, diagnostics },
        };
        let mut builder = PatchBuilder::new(db);
        builder.add_str("{\n");
        builder.add_str(&format!(
            "    let mut {FORMATTER_NAME}: core::fmt::Formatter = \
             core::traits::Default::default();\n"
        ));
        if let Err(diag) = info.add_to_formatter(&mut builder, false) {
            return InlinePluginResult { code: None, diagnostics: vec![diag] };
        }
        builder.add_str(&format!("    {FORMATTER_NAME}.buffer\n"));
        builder.add_str("}\n");
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: format!("{}_macro", Self::NAME).into(),
                content: builder.code,
                diagnostics_mappings: builder.diagnostics_mappings,
                aux_data: None,
            }),
            diagnostics: vec![],
        }
    }
}
