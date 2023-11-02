use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{InlineMacroExprPlugin, InlinePluginResult, PluginGeneratedFile};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;

use crate::inline_macros::formatted::FormattedInfo;

/// Macro for printing.
#[derive(Debug, Default)]
pub struct PrintMacro;
impl PrintMacro {
    pub const NAME: &'static str = "print";
}
impl InlineMacroExprPlugin for PrintMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        generate_code_inner(syntax, db, false)
    }
}

/// Macro for printing with a new line.
#[derive(Debug, Default)]
pub struct PrintlnMacro;
impl PrintlnMacro {
    pub const NAME: &'static str = "println";
}
impl InlineMacroExprPlugin for PrintlnMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        generate_code_inner(syntax, db, true)
    }
}

fn generate_code_inner(
    syntax: &ast::ExprInlineMacro,
    db: &dyn SyntaxGroup,
    with_newline: bool,
) -> InlinePluginResult {
    const FORMATTER_NAME: &str = "__formatter_for_print_macros_";
    let info = match FormattedInfo::extract(db, syntax, Some(FORMATTER_NAME)) {
        Ok(info) => info,
        Err(diagnostics) => return InlinePluginResult { code: None, diagnostics },
    };
    let mut builder = PatchBuilder::new(db);
    builder.add_str("{\n");
    builder.add_str(&format!(
        "    let mut {FORMATTER_NAME}: core::fmt::Formatter = core::traits::Default::default();\n"
    ));
    if let Err(diag) = info.add_to_formatter(&mut builder, false) {
        return InlinePluginResult { code: None, diagnostics: vec![diag] };
    }
    builder.add_str(&format!(
        "    core::debug::print_byte_array_as_string(@{FORMATTER_NAME}.buffer);\n"
    ));
    builder.add_str("}\n");
    InlinePluginResult {
        code: Some(PluginGeneratedFile {
            name: format!("{}_macro", get_macro_name(with_newline)).into(),
            content: builder.code,
            diagnostics_mappings: builder.diagnostics_mappings,
            aux_data: None,
        }),
        diagnostics: vec![],
    }
}

/// Gets the macro name according to the `with_newline` flag.
fn get_macro_name(with_newline: bool) -> &'static str {
    if with_newline { PrintlnMacro::NAME } else { PrintMacro::NAME }
}
