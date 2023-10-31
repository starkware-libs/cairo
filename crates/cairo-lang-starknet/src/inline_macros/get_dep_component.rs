use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_semantic::inline_macros::unsupported_bracket_diagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use itertools::Itertools;

/// Macro for getting a component given a contract state that has it.
#[derive(Debug, Default)]
pub struct GetDepComponentMacro;
impl GetDepComponentMacro {
    pub const NAME: &'static str = "get_dep_component";
}
impl InlineMacroExprPlugin for GetDepComponentMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        get_dep_component_generate_code_helper(db, syntax, false)
    }
}

/// Macro for getting a mutable component given a mutable contract state that has it.
#[derive(Debug, Default)]
pub struct GetDepComponentMutMacro;
impl GetDepComponentMutMacro {
    pub const NAME: &'static str = "get_dep_component_mut";
}
impl InlineMacroExprPlugin for GetDepComponentMutMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        get_dep_component_generate_code_helper(db, syntax, true)
    }
}

/// A helper function for the code generation of both `DepComponentMacro` and
/// `DepComponentMutMacro`. `is_mut` selects between the two.
fn get_dep_component_generate_code_helper(
    db: &dyn SyntaxGroup,
    syntax: &ast::ExprInlineMacro,
    is_mut: bool,
) -> InlinePluginResult {
    let macro_args = if let ast::WrappedArgList::ParenthesizedArgList(args) = syntax.arguments(db) {
        args.arguments(db)
    } else {
        return unsupported_bracket_diagnostic(db, syntax);
    };

    let arguments = macro_args.elements(db).iter().map(|arg| arg.arg_clause(db)).collect_vec();
    let [ast::ArgClause::Unnamed(contract_arg), ast::ArgClause::Unnamed(component_impl_arg)] =
        arguments.as_slice()
    else {
        let diagnostics = vec![PluginDiagnostic {
            stable_ptr: syntax.stable_ptr().untyped(),
            message: format!(
                "`{}` macro must have exactly two unnamed arguments.",
                if is_mut { GetDepComponentMutMacro::NAME } else { GetDepComponentMacro::NAME }
            ),
        }];
        return InlinePluginResult { code: None, diagnostics };
    };
    if is_mut {
        // Verify the first element has  only a `ref` modifier.
        let contract_arg_modifiers = macro_args.elements(db)[0].modifiers(db).elements(db);
        if !matches!(&contract_arg_modifiers[..], &[ast::Modifier::Ref(_)]) {
            // TODO(Gil): The generated diagnostics points to the whole inline macro, it should
            // point to the arg.
            let diagnostics = vec![PluginDiagnostic {
                stable_ptr: contract_arg.stable_ptr().untyped(),
                message: format!(
                    "The first argument of `{}` macro must have only a `ref` modifier.",
                    GetDepComponentMutMacro::NAME
                ),
            }];
            return InlinePluginResult { code: None, diagnostics };
        };
    }
    let mut builder = PatchBuilder::new(db);
    let (let_part, maybe_mut, ref_or_snapshot, maybe_ref) =
        if is_mut { ("let mut", "_mut", "ref ", "ref ") } else { ("let", "", "@", "") };
    builder.add_modified(RewriteNode::interpolate_patched(
        &format!(
        "
            {{
                {let_part} __get_dep_component_macro_temp_contract__ = \
         HasComponent::get_contract{maybe_mut}({ref_or_snapshot}$contract_path$);
                $component_impl_path$::get_component{maybe_mut}({maybe_ref}\
         __get_dep_component_macro_temp_contract__)
            }}
            "),
        &[
            (
                "contract_path".to_string(),
                RewriteNode::new_trimmed(contract_arg.value(db).as_syntax_node()),
            ),
            (
                "component_impl_path".to_string(),
                RewriteNode::new_trimmed(component_impl_arg.value(db).as_syntax_node()),
            ),
        ]
        .into(),
    ));

    InlinePluginResult {
        code: Some(PluginGeneratedFile {
            name: "get_dep_component_inline_macro".into(),
            content: builder.code,
            diagnostics_mappings: builder.diagnostics_mappings,
            aux_data: None,
        }),
        diagnostics: vec![],
    }
}
