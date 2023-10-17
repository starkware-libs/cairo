use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_semantic::inline_macros::{try_extract_unnamed_arg, unsupported_bracket_diagnostic};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

/// Macro for expanding a selector to a string literal.
#[derive(Debug, Default)]
pub struct DepComponentMacro;
impl DepComponentMacro {
    pub const NAME: &'static str = "dep_component";
}
impl InlineMacroExprPlugin for DepComponentMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        dep_component_generate_code_helper(db, syntax, false)
    }
}

#[derive(Debug, Default)]
pub struct DepComponentMutMacro;
impl DepComponentMutMacro {
    pub const NAME: &'static str = "dep_component_mut";
}
impl InlineMacroExprPlugin for DepComponentMutMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        dep_component_generate_code_helper(db, syntax, true)
    }
}

/// An helper function for the code generation of both `DepComponentMacro` and
/// `DepComponentMutMacro`. `is_mut` selects between the two.
fn dep_component_generate_code_helper(
    db: &dyn SyntaxGroup,
    syntax: &ast::ExprInlineMacro,
    is_mut: bool,
) -> InlinePluginResult {
    let macro_args = if let ast::WrappedArgList::ParenthesizedArgList(args) = syntax.arguments(db) {
        args.args(db)
    } else {
        return unsupported_bracket_diagnostic(db, syntax);
    };

    let arguments = macro_args.elements(db);
    let [contract_arg, component_arg] = arguments.as_slice() else {
        let diagnostics = vec![PluginDiagnostic {
            stable_ptr: syntax.stable_ptr().untyped(),
            message: format!(
                "`{}` macro must have exactly two unnamed argument.",
                if is_mut { DepComponentMutMacro::NAME } else { DepComponentMacro::NAME }
            ),
        }];
        return InlinePluginResult { code: None, diagnostics };
    };

    let (Some(contract_path), Some(component_path)) =
        (try_extract_unnamed_arg(db, contract_arg), try_extract_unnamed_arg(db, component_arg))
    else {
        let diagnostics = vec![PluginDiagnostic {
            stable_ptr: syntax.stable_ptr().untyped(),
            message: format!(
                "`{}` macro must have exactly two unnamed argument.",
                if is_mut { DepComponentMutMacro::NAME } else { DepComponentMacro::NAME }
            ),
        }];
        return InlinePluginResult { code: None, diagnostics };
    };

    let mut builder = PatchBuilder::new(db);
    let let_part = if is_mut { "let mut" } else { "let" };
    let maybe_mut = if is_mut { "_mut" } else { "" };
    let maybe_ref = if is_mut { "ref " } else { "" };
    builder.add_modified(RewriteNode::interpolate_patched(
        &format!(
        "
            {{
                {let_part} __get_dep_component_macro_temp_contract__ = \
         HasComponent::get_contract{maybe_mut}({maybe_ref}$contract_path$);
                $component_path$::HasComponent::get_component{maybe_mut}({maybe_ref}\
         __get_dep_component_macro_temp_contract__)
            }}
            "),
        &[
            ("contract_path".to_string(), RewriteNode::new_trimmed(contract_path.as_syntax_node())),
            (
                "component_path".to_string(),
                RewriteNode::new_trimmed(component_path.as_syntax_node()),
            ),
        ]
        .into(),
    ));

    InlinePluginResult {
        code: Some(PluginGeneratedFile {
            name: "dep_component_mut_inline_macro".into(),
            content: builder.code,
            diagnostics_mappings: builder.diagnostics_mappings,
            aux_data: None,
        }),
        diagnostics: vec![],
    }
}
