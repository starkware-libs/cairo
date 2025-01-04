use cairo_lang_defs::extract_macro_unnamed_args;
use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin, PluginDiagnostic,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{PluginResultTrait, not_legacy_macro_diagnostic};
use cairo_lang_parser::macro_helpers::AsLegacyInlineMacro;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use cairo_lang_utils::extract_matches;

/// Macro for getting a component given a contract state that has it.
#[derive(Debug, Default)]
pub struct GetDepComponentMacro;
impl NamedPlugin for GetDepComponentMacro {
    const NAME: &'static str = "get_dep_component";
}
impl InlineMacroExprPlugin for GetDepComponentMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        get_dep_component_generate_code_helper(db, syntax, false)
    }
}

/// Macro for getting a mutable component given a mutable contract state that has it.
#[derive(Debug, Default)]
pub struct GetDepComponentMutMacro;
impl NamedPlugin for GetDepComponentMutMacro {
    const NAME: &'static str = "get_dep_component_mut";
}
impl InlineMacroExprPlugin for GetDepComponentMutMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        _metadata: &MacroPluginMetadata<'_>,
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
    let Some(legacy_inline_macro) = syntax.as_legacy_inline_macro(db) else {
        return InlinePluginResult::diagnostic_only(not_legacy_macro_diagnostic(
            syntax.as_syntax_node().stable_ptr(),
        ));
    };
    let [contract_arg, component_impl_arg] = extract_macro_unnamed_args!(
        db,
        &legacy_inline_macro,
        2,
        ast::WrappedArgList::ParenthesizedArgList(_),
        syntax.stable_ptr()
    );

    if is_mut {
        // `extract_macro_unnamed_args` above guarantees that we have `ParenthesizedArgList`.
        let contract_arg_modifiers = extract_matches!(
            legacy_inline_macro.arguments(db),
            ast::WrappedArgList::ParenthesizedArgList
        )
        .arguments(db)
        .elements(db)[0]
            .modifiers(db)
            .elements(db);

        // Verify the first element has only a `ref` modifier.
        if !matches!(&contract_arg_modifiers[..], &[ast::Modifier::Ref(_)]) {
            // TODO(Gil): The generated diagnostics points to the whole inline macro, it should
            // point to the arg.
            let diagnostics = vec![PluginDiagnostic::error_with_inner_span(
                db,
                syntax.stable_ptr(),
                contract_arg.as_syntax_node(),
                format!(
                    "The first argument of `{}` macro must have only a `ref` modifier.",
                    GetDepComponentMutMacro::NAME
                ),
            )];
            return InlinePluginResult { code: None, diagnostics };
        };
    }
    let mut builder = PatchBuilder::new(db, syntax);
    let (let_part, maybe_mut, maybe_ref) =
        if is_mut { ("let mut", "_mut", "ref ") } else { ("let", "", "") };
    builder.add_modified(RewriteNode::interpolate_patched(
        &format!(
            "
            {{
                {let_part} __get_dep_component_macro_temp_contract__ = \
         HasComponent::get_contract{maybe_mut}({maybe_ref}$contract_path$);
                $component_impl_path$::get_component{maybe_mut}({maybe_ref}\
         __get_dep_component_macro_temp_contract__)
            }}
            "
        ),
        &[
            ("contract_path".to_string(), RewriteNode::new_trimmed(contract_arg.as_syntax_node())),
            (
                "component_impl_path".to_string(),
                RewriteNode::new_trimmed(component_impl_arg.as_syntax_node()),
            ),
        ]
        .into(),
    ));

    let (content, code_mappings) = builder.build();
    InlinePluginResult {
        code: Some(PluginGeneratedFile {
            name: "get_dep_component_inline_macro".into(),
            content,
            code_mappings,
            aux_data: None,
        }),
        diagnostics: vec![],
    }
}
