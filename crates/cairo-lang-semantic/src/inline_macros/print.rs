use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use indoc::formatdoc;

use crate::extract_macro_unnamed_args;

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
    let [format_string, arg] =
        extract_macro_unnamed_args!(db, syntax, 2, ast::WrappedArgList::ParenthesizedArgList(_));

    if format_string.as_syntax_node().get_text(db) != r#""{}""# {
        let diagnostics = vec![PluginDiagnostic {
            stable_ptr: syntax.stable_ptr().untyped(),
            message: format!(
                r#"Currently, the `{}` macro only supports `"{{}}"` as the first argument (format string)."#,
                get_macro_name(with_newline)
            ),
        }];
        return InlinePluginResult { code: None, diagnostics };
    }

    let (maybe_mut, maybe_append_newline) = if with_newline {
        (" mut", "core::byte_array::ByteArrayImpl::append_byte(ref ba, 0x0a_u8);")
    } else {
        ("", "")
    };
    let mut builder = PatchBuilder::new(db);
    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "{{
                let{maybe_mut} ba = format!($format_string$, $arg$);
                {maybe_append_newline}
                debug::print_byte_array_as_string(@ba);
            }}"
        ),
        &[
            ("format_string".to_string(), RewriteNode::new_trimmed(format_string.as_syntax_node())),
            ("arg".to_string(), RewriteNode::new_trimmed(arg.as_syntax_node())),
        ]
        .into(),
    ));

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
