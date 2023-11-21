use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, NamedPlugin, PluginGeneratedFile,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::WrappedArgListHelper;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use indoc::formatdoc;

use super::write::{WriteMacro, WritelnMacro};

/// Macro for printing.
#[derive(Debug, Default)]
pub struct PrintMacro;
impl NamedPlugin for PrintMacro {
    const NAME: &'static str = "print";
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
impl NamedPlugin for PrintlnMacro {
    const NAME: &'static str = "println";
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
    let arguments = syntax.arguments(db);
    let mut builder = PatchBuilder::new(db);
    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc! {
            "
                {{
                    let mut {f}: core::fmt::Formatter = core::traits::Default::default();
                    core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                        {write_func}!$left_bracket${f}, $args$$right_bracket$
                    );
                    core::debug::print_byte_array_as_string(@{f}.buffer);
                }}
            ",
            f = "__formatter_for_print_macros__",
            write_func = if with_newline { WritelnMacro::NAME } else { WriteMacro::NAME },
        },
        &[
            (
                "left_bracket".to_string(),
                RewriteNode::new_trimmed(arguments.left_bracket_syntax_node(db)),
            ),
            (
                "right_bracket".to_string(),
                RewriteNode::new_trimmed(arguments.right_bracket_syntax_node(db)),
            ),
            (
                "args".to_string(),
                RewriteNode::new_trimmed(arguments.arg_list(db).unwrap().as_syntax_node()),
            ),
        ]
        .into(),
    ));
    InlinePluginResult {
        code: Some(PluginGeneratedFile {
            name: format!("{}_macro", get_macro_name(with_newline)).into(),
            content: builder.code,
            code_mappings: builder.code_mappings,
            aux_data: None,
        }),
        diagnostics: vec![],
    }
}

/// Gets the macro name according to the `with_newline` flag.
fn get_macro_name(with_newline: bool) -> &'static str {
    if with_newline { PrintlnMacro::NAME } else { PrintMacro::NAME }
}
