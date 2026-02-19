use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_plugins::plugins::HIDDEN_ATTR_SYNTAX;
use cairo_lang_semantic::keyword::SELF_PARAM_KW;
use cairo_lang_syntax::attribute::consts::IMPLICIT_PRECEDENCE_ATTR;
use cairo_lang_syntax::node::ast::{
    self, Attribute, FunctionWithBody, OptionReturnTypeClause, OptionTypeClause,
    OptionWrappedGenericParamList,
};
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::extract_matches;
use indoc::{formatdoc, indoc};
use itertools::Itertools;
use salsa::Database;

use super::consts::{
    CONSTRUCTOR_ATTR, CONSTRUCTOR_MODULE, CONSTRUCTOR_NAME, EXTERNAL_ATTR, EXTERNAL_MODULE,
    IMPLICIT_PRECEDENCE, L1_HANDLER_ATTR, L1_HANDLER_FIRST_PARAM_NAME, L1_HANDLER_MODULE,
    RAW_OUTPUT_ATTR, WRAPPER_PREFIX,
};
use super::utils::{AstPathExtract, ParamEx, find_v0_attribute, maybe_strip_underscore};

/// Kind of an entry point. Determined by the entry point's attributes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryPointKind {
    External,
    Constructor,
    L1Handler,
}

/// Helper trait for entry point kind extraction.
pub trait GetEntryPointKind<'db> {
    /// Returns the entry point kind and its trigger attribute if the attributes mark it as an entry
    /// point.
    fn entry_point_kind(
        &self,
        db: &'db dyn Database,
        diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    ) -> Option<(EntryPointKind, Attribute<'db>)>;
}

impl<'db> GetEntryPointKind<'db> for FunctionWithBody<'db> {
    fn entry_point_kind(
        &self,
        db: &'db dyn Database,
        diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    ) -> Option<(EntryPointKind, Attribute<'db>)> {
        if let Some(trigger) = find_v0_attribute(db, diagnostics, self, EXTERNAL_ATTR) {
            Some((EntryPointKind::External, trigger))
        } else if let Some(trigger) = self.find_attr(db, CONSTRUCTOR_ATTR) {
            Some((EntryPointKind::Constructor, trigger))
        } else {
            self.find_attr(db, L1_HANDLER_ATTR).map(|trigger| (EntryPointKind::L1Handler, trigger))
        }
    }
}

// Accumulated data for generation of contract entry points.
#[derive(Default)]
pub struct EntryPointsGenerationData<'db> {
    pub generated_wrapper_functions: Vec<RewriteNode<'db>>,
    pub external_functions: Vec<RewriteNode<'db>>,
    pub constructor_functions: Vec<RewriteNode<'db>>,
    pub l1_handler_functions: Vec<RewriteNode<'db>>,
}
impl<'db> EntryPointsGenerationData<'db> {
    pub fn into_rewrite_node(self) -> RewriteNode<'db> {
        let generated_external_module =
            generate_submodule(EXTERNAL_MODULE, RewriteNode::new_modified(self.external_functions));
        let generated_l1_handler_module = generate_submodule(
            L1_HANDLER_MODULE,
            RewriteNode::new_modified(self.l1_handler_functions),
        );
        let generated_constructor_module = generate_submodule(
            CONSTRUCTOR_MODULE,
            RewriteNode::new_modified(self.constructor_functions),
        );
        RewriteNode::interpolate_patched(
            indoc! {"
                $generated_wrapper_functions$
                $generated_external_module$
                $generated_l1_handler_module$
                $generated_constructor_module$"},
            &[
                (
                    "generated_wrapper_functions".to_string(),
                    RewriteNode::new_modified(self.generated_wrapper_functions),
                ),
                ("generated_external_module".to_string(), generated_external_module),
                ("generated_l1_handler_module".to_string(), generated_l1_handler_module),
                ("generated_constructor_module".to_string(), generated_constructor_module),
            ]
            .into(),
        )
    }
}

/// Generates a submodule with the given name, uses and functions.
fn generate_submodule<'db>(
    module_name: &str,
    generated_functions_node: RewriteNode<'db>,
) -> RewriteNode<'db> {
    RewriteNode::interpolate_patched(
        &formatdoc! {"
            {HIDDEN_ATTR_SYNTAX}
            pub mod {module_name} {{$generated_functions_node$
            }}"
        },
        &[("generated_functions_node".to_string(), generated_functions_node)].into(),
    )
}

/// Parameters for generating an entry point, used when calling `handle_entry_point`.
pub struct EntryPointGenerationParams<'db, 'a> {
    pub trigger_attribute: Attribute<'db>,
    pub entry_point_kind: EntryPointKind,
    pub item_function: &'a FunctionWithBody<'db>,
    pub wrapped_function_path: RewriteNode<'db>,
    pub wrapper_identifier: String,
    pub unsafe_new_contract_state_prefix: &'a str,
    pub generic_params: RewriteNode<'db>,
}

/// Handles a contract entrypoint function.
pub fn handle_entry_point<'db, 'a>(
    db: &'db dyn Database,
    EntryPointGenerationParams {
        trigger_attribute,
        entry_point_kind,
        item_function,
        wrapped_function_path,
        wrapper_identifier,
        unsafe_new_contract_state_prefix,
        generic_params,
    }: EntryPointGenerationParams<'db, 'a>,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    data: &mut EntryPointsGenerationData<'db>,
) {
    let declaration = item_function.declaration(db);
    let name_node = declaration.name(db);
    let function_name = name_node.text(db).long(db);
    if entry_point_kind == EntryPointKind::Constructor && function_name != CONSTRUCTOR_NAME {
        diagnostics.push(PluginDiagnostic::error(
            name_node.stable_ptr(db),
            format!("The constructor function must be called `{CONSTRUCTOR_NAME}`."),
        ));
    }

    if let OptionWrappedGenericParamList::WrappedGenericParamList(generic_params) =
        declaration.generic_params(db)
    {
        diagnostics.push(PluginDiagnostic::error(
            generic_params.stable_ptr(db),
            "Contract entry points cannot have generic arguments".to_string(),
        ))
    }

    let params = declaration.signature(db).parameters(db);
    let wrapper_function_name = &format!("{WRAPPER_PREFIX}{wrapper_identifier}");
    match generate_entry_point_wrapper(
        db,
        item_function,
        wrapped_function_path,
        RewriteNode::text(wrapper_function_name),
        generic_params,
        unsafe_new_contract_state_prefix,
    ) {
        Ok(generated_function) => {
            data.generated_wrapper_functions
                .push(generated_function.mapped(db, &trigger_attribute));
            data.generated_wrapper_functions.push(RewriteNode::text("\n"));
            let generated = match entry_point_kind {
                EntryPointKind::Constructor => &mut data.constructor_functions,
                EntryPointKind::L1Handler => {
                    validate_l1_handler_second_parameter(db, &params, diagnostics);
                    &mut data.l1_handler_functions
                }
                EntryPointKind::External => &mut data.external_functions,
            };
            generated.push(
                RewriteNode::text(&format!(
                    "\n    pub use super::{wrapper_function_name} as {function_name};",
                ))
                .mapped(db, &trigger_attribute),
            );
        }
        Err(entry_point_diagnostics) => {
            diagnostics.extend(entry_point_diagnostics);
        }
    }
}

/// Generates Cairo code for an entry point wrapper.
fn generate_entry_point_wrapper<'db>(
    db: &'db dyn Database,
    function: &FunctionWithBody<'db>,
    wrapped_function_path: RewriteNode<'db>,
    wrapper_function_name: RewriteNode<'db>,
    generic_params: RewriteNode<'db>,
    unsafe_new_contract_state_prefix: &str,
) -> Result<RewriteNode<'db>, Vec<PluginDiagnostic<'db>>> {
    let declaration = function.declaration(db);
    let sig = declaration.signature(db);
    let sig_params = sig.parameters(db);
    let mut params = sig_params.elements(db).enumerate();
    let mut diagnostics = vec![];
    let mut arg_names = Vec::new();
    let mut arg_definitions = Vec::new();
    let mut ref_appends = Vec::new();

    let Some((0, first_param)) = params.next() else {
        return Err(vec![PluginDiagnostic::error(
            sig.stable_ptr(db),
            "The first parameter of an entry point must be `self`.".into(),
        )]);
    };
    if first_param.name(db).text(db).long(db) != SELF_PARAM_KW {
        return Err(vec![PluginDiagnostic::error(
            first_param.stable_ptr(db),
            "The first parameter of an entry point must be `self`.".into(),
        )]);
    };
    let is_snapshot = matches!(
        extract_matches!(first_param.type_clause(db), OptionTypeClause::TypeClause).ty(db),
        ast::Expr::Unary(_)
    );
    // TODO(spapini): Check modifiers and type.

    let raw_output = function.has_attr(db, RAW_OUTPUT_ATTR);
    for (param_idx, param) in params {
        let arg_name = format!("__arg_{}", param.name(db).text(db).long(db));
        let arg_type_ast =
            extract_matches!(param.type_clause(db), OptionTypeClause::TypeClause).ty(db);
        let type_name = arg_type_ast.as_syntax_node().get_text_without_trivia(db).long(db);

        let is_ref = param.is_ref_param(db);
        if raw_output && is_ref {
            diagnostics.push(PluginDiagnostic::error(
                param.modifiers(db).stable_ptr(db),
                format!("`{RAW_OUTPUT_ATTR}` functions cannot have `ref` parameters."),
            ));
        }
        let ref_modifier = if is_ref { "ref " } else { "" };
        arg_names.push(format!("{ref_modifier}{arg_name}"));
        let mut_modifier = if is_ref { "mut " } else { "" };
        let arg_definition = formatdoc!(
            "
            let {mut_modifier}{arg_name} = core::option::OptionTraitImpl::expect(
                    core::serde::Serde::<{type_name}>::deserialize(ref data),
                    'Failed to deserialize param #{param_idx}'
                );"
        );
        arg_definitions.push(arg_definition);

        if is_ref {
            ref_appends.push(RewriteNode::Text(format!(
                "\n            core::serde::Serde::<{type_name}>::serialize(@{arg_name}, ref arr);"
            )));
        }
    }
    let arg_names_str = arg_names.join(", ");

    let ret_ty = sig.ret_ty(db);
    let (let_res, append_res, return_ty_is_felt252_span, ret_type_ptr) = match &ret_ty {
        OptionReturnTypeClause::Empty(type_clause_ast) => {
            ("", "".to_string(), false, type_clause_ast.stable_ptr(db).untyped())
        }
        OptionReturnTypeClause::ReturnTypeClause(ty) => {
            let ret_type_ast = ty.ty(db);

            let return_ty_is_felt252_span = ret_type_ast.is_felt252_span(db);
            let ret_type_name = ret_type_ast.as_syntax_node().get_text_without_trivia(db).long(db);
            (
                "let res = ",
                format!("\n    core::serde::Serde::<{ret_type_name}>::serialize(@res, ref arr);"),
                return_ty_is_felt252_span,
                ret_type_ast.stable_ptr(db).untyped(),
            )
        }
    };

    if raw_output && !return_ty_is_felt252_span {
        diagnostics.push(PluginDiagnostic::error(
            ret_type_ptr,
            format!("`{RAW_OUTPUT_ATTR}` functions must return `Span::<felt252>`."),
        ));
    }

    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    let contract_state_arg = if is_snapshot { "@contract_state" } else { "ref contract_state" };
    let output_handling_string = if raw_output {
        format!("$wrapped_function_path$({contract_state_arg}, {arg_names_str})")
    } else {
        formatdoc! {"
            {let_res}$wrapped_function_path$({contract_state_arg}, {arg_names_str});
                let mut arr = ArrayTrait::new();
                // References.$ref_appends$
                // Result.{append_res}
                core::array::ArrayTrait::span(@arr)"
        }
    };

    let output_handling = RewriteNode::interpolate_patched(
        &output_handling_string,
        &[
            ("wrapped_function_path".to_string(), wrapped_function_path),
            ("ref_appends".to_string(), RewriteNode::new_modified(ref_appends)),
        ]
        .into(),
    );

    let implicit_precedence = RewriteNode::Text(format!("#[{IMPLICIT_PRECEDENCE_ATTR}({})]", {
        IMPLICIT_PRECEDENCE.iter().join(", ")
    }));

    let arg_definitions = RewriteNode::Text(arg_definitions.join("\n    "));
    Ok(RewriteNode::interpolate_patched(
        &formatdoc! {"
            #[doc(hidden)]
            $implicit_precedence$
            fn $wrapper_function_name$$generic_params$(mut data: Span::<felt252>) -> Span::<felt252> {{
                core::internal::require_implicit::<System>();
                core::internal::revoke_ap_tracking();
                let Some(_) = core::gas::withdraw_gas() else {{
                    core::panic_with_felt252('Out of gas');
                }};
                $arg_definitions$
                assert(core::array::SpanTrait::is_empty(data), 'Input too long for arguments');
                let Some(_) = core::gas::withdraw_gas_all(core::gas::get_builtin_costs()) else {{
                    core::panic_with_felt252('Out of gas');
                }};
                let mut contract_state = {unsafe_new_contract_state_prefix}unsafe_new_contract_state();
                $output_handling$
            }}
        "},
        &[
            ("wrapper_function_name".to_string(), wrapper_function_name),
            ("generic_params".to_string(), generic_params),
            ("output_handling".to_string(), output_handling),
            ("arg_definitions".to_string(), arg_definitions),
            ("implicit_precedence".to_string(), implicit_precedence),
        ]
        .into(),
    ))
}

/// Validates the second parameter of an L1 handler is `from_address: felt252` or `_from_address:
/// felt252`.
fn validate_l1_handler_second_parameter<'db>(
    db: &'db dyn Database,
    params: &ast::ParamList<'db>,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) {
    if let Some(second_param) = params.elements(db).nth(1) {
        // Validate type

        if !extract_matches!(second_param.type_clause(db), OptionTypeClause::TypeClause)
            .ty(db)
            .is_felt252(db)
        {
            diagnostics.push(PluginDiagnostic::error(
                second_param.stable_ptr(db),
                "The second parameter of an L1 handler must be of type `felt252`.".to_string(),
            ));
        }

        // Validate name
        if maybe_strip_underscore(second_param.name(db).text(db).long(db).as_str())
            != L1_HANDLER_FIRST_PARAM_NAME
        {
            diagnostics.push(PluginDiagnostic::error(
                second_param.stable_ptr(db),
                "The second parameter of an L1 handler must be named 'from_address'.".to_string(),
            ));
        }
    } else {
        diagnostics.push(PluginDiagnostic::error(
            params.stable_ptr(db),
            "An L1 handler must have the 'from_address' as its second parameter.".to_string(),
        ));
    };
}
