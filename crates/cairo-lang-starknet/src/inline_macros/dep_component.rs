use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_semantic::inline_macros::unsupported_bracket_diagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use itertools::Itertools;

use crate::contract;

/// Macro for getting a component given a contract state that has it.
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

/// Macro for getting a mutable component given a contract state that has it.
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

/// A helper function for the code generation of both `DepComponentMacro` and
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

    let arguments = macro_args.elements(db).iter().map(|arg| arg.arg_clause(db)).collect_vec();
    let [ast::ArgClause::Unnamed(contract_arg), ast::ArgClause::Unnamed(component_module_arg)] =
        arguments.as_slice()
    else {
        let diagnostics = vec![PluginDiagnostic {
            stable_ptr: syntax.stable_ptr().untyped(),
            message: format!(
                "`{}` macro must have exactly two unnamed arguments.",
                if is_mut { DepComponentMutMacro::NAME } else { DepComponentMacro::NAME }
            ),
        }];
        return InlinePluginResult { code: None, diagnostics };
    };

    let mut builder = PatchBuilder::new(db);
    let (let_part, maybe_mut, ref_or_snapshot, maybe_ref) =
        if is_mut { ("let mut", "_mut", "ref ", "ref ") } else { ("let", "", "@", "") };
    builder.add_modified(RewriteNode::interpolate_patched(
        &format!(
        "
            {{
                {let_part} __get_dep_component_macro_temp_contract__ = \
         HasComponent::<TContractState>::get_contract{maybe_mut}({ref_or_snapshot}$contract_path$);
                $component_module_path$::HasComponent::<TContractState>::get_component{maybe_mut}({maybe_ref}\
         __get_dep_component_macro_temp_contract__)
            }}
            "),
        &[
            ("contract_path".to_string(), RewriteNode::new_trimmed(contract_arg.value(db).as_syntax_node())),
            (
                "component_module_path".to_string(),
                RewriteNode::new_trimmed(component_module_arg.value(db).as_syntax_node()),
            ),
        ]
        .into(),
    ));

    InlinePluginResult {
        code: Some(PluginGeneratedFile {
            name: "dep_component_inline_macro".into(),
            content: builder.code,
            diagnostics_mappings: builder.diagnostics_mappings,
            aux_data: None,
        }),
        diagnostics: vec![],
    }
}
