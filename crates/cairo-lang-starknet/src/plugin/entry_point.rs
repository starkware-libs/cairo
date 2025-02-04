use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::attribute::consts::IMPLICIT_PRECEDENCE_ATTR;
use cairo_lang_syntax::node::ast::{
    self, FunctionWithBody, OptionReturnTypeClause, OptionTypeClause, OptionWrappedGenericParamList,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::extract_matches;
use indoc::{formatdoc, indoc};
use itertools::Itertools;

use super::consts::{
    CONSTRUCTOR_ATTR, CONSTRUCTOR_MODULE, CONSTRUCTOR_NAME, EXTERNAL_ATTR, EXTERNAL_MODULE,
    IMPLICIT_PRECEDENCE, L1_HANDLER_ATTR, L1_HANDLER_FIRST_PARAM_NAME, L1_HANDLER_MODULE,
    RAW_OUTPUT_ATTR, WRAPPER_PREFIX,
};
use super::utils::{AstPathExtract, ParamEx, has_v0_attribute, maybe_strip_underscore};

/// Kind of an entry point. Determined by the entry point's attributes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryPointKind {
    External,
    Constructor,
    L1Handler,
}
impl EntryPointKind {
    /// Returns the entry point kind if the given function is indeed marked as an entry point.
    pub fn try_from_function_with_body(
        db: &dyn SyntaxGroup,
        diagnostics: &mut Vec<PluginDiagnostic>,
        item_function: &FunctionWithBody,
    ) -> Option<Self> {
        if has_v0_attribute(
            db,
            diagnostics,
            &ast::ModuleItem::FreeFunction(item_function.clone()),
            EXTERNAL_ATTR,
        ) {
            Some(EntryPointKind::External)
        } else if item_function.has_attr(db, CONSTRUCTOR_ATTR) {
            Some(EntryPointKind::Constructor)
        } else if item_function.has_attr(db, L1_HANDLER_ATTR) {
            Some(EntryPointKind::L1Handler)
        } else {
            None
        }
    }

    /// Returns the entry point kind if the attributes mark it as an entry point.
    pub fn try_from_attrs(
        db: &dyn SyntaxGroup,
        diagnostics: &mut Vec<PluginDiagnostic>,
        attrs: &impl QueryAttrs,
    ) -> Option<Self> {
        if has_v0_attribute(db, diagnostics, attrs, EXTERNAL_ATTR) {
            Some(EntryPointKind::External)
        } else if attrs.has_attr(db, CONSTRUCTOR_ATTR) {
            Some(EntryPointKind::Constructor)
        } else if attrs.has_attr(db, L1_HANDLER_ATTR) {
            Some(EntryPointKind::L1Handler)
        } else {
            None
        }
    }
}

// Accumulated data for generation of contract entry points.
#[derive(Default)]
pub struct EntryPointsGenerationData {
    pub generated_wrapper_functions: Vec<RewriteNode>,
    pub external_functions: Vec<RewriteNode>,
    pub constructor_functions: Vec<RewriteNode>,
    pub l1_handler_functions: Vec<RewriteNode>,
}
impl EntryPointsGenerationData {
    pub fn into_rewrite_node(self) -> RewriteNode {
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
fn generate_submodule(module_name: &str, generated_functions_node: RewriteNode) -> RewriteNode {
    RewriteNode::interpolate_patched(
        &formatdoc! {"
            pub mod {module_name} {{$generated_functions_node$
            }}"
        },
        &[("generated_functions_node".to_string(), generated_functions_node)].into(),
    )
}

/// Parameters for generating an entry point, used when calling `handle_entry_point`.
pub struct EntryPointGenerationParams<'a> {
    pub entry_point_kind: EntryPointKind,
    pub item_function: &'a FunctionWithBody,
    pub wrapped_function_path: RewriteNode,
    pub wrapper_identifier: String,
    pub unsafe_new_contract_state_prefix: &'a str,
    pub generic_params: RewriteNode,
}

/// Handles a contract entrypoint function.
pub fn handle_entry_point(
    db: &dyn SyntaxGroup,
    EntryPointGenerationParams {
        entry_point_kind,
        item_function,
        wrapped_function_path,
        wrapper_identifier,
        unsafe_new_contract_state_prefix,
        generic_params,
    }: EntryPointGenerationParams<'_>,
    diagnostics: &mut Vec<PluginDiagnostic>,
    data: &mut EntryPointsGenerationData,
) {
    let declaration = item_function.declaration(db);
    let name_node = declaration.name(db);
    if entry_point_kind == EntryPointKind::Constructor && name_node.text(db) != CONSTRUCTOR_NAME {
        diagnostics.push(PluginDiagnostic::error(
            name_node.stable_ptr().untyped(),
            format!("The constructor function must be called `{CONSTRUCTOR_NAME}`."),
        ));
    }

    if let OptionWrappedGenericParamList::WrappedGenericParamList(generic_params) =
        declaration.generic_params(db)
    {
        diagnostics.push(PluginDiagnostic::error(
            generic_params.stable_ptr().untyped(),
            "Contract entry points cannot have generic arguments".to_string(),
        ))
    }

    let mut declaration_node = RewriteNode::from_ast_trimmed(&declaration);
    let original_parameters = declaration_node
        .modify_child(db, ast::FunctionDeclaration::INDEX_SIGNATURE)
        .modify_child(db, ast::FunctionSignature::INDEX_PARAMETERS);
    let params = declaration.signature(db).parameters(db);
    for (param_idx, param) in params.elements(db).iter().enumerate() {
        // This assumes `mut` can only appear alone.
        if param.is_mut_param(db) {
            original_parameters
                .modify_child(db, param_idx * 2)
                .modify_child(db, ast::Param::INDEX_MODIFIERS)
                .set_str("".to_string());
        }
    }
    let function_name = RewriteNode::from_ast_trimmed(&name_node);
    let wrapper_function_name = RewriteNode::interpolate_patched(
        &format!("{WRAPPER_PREFIX}{wrapper_identifier}"),
        &[("function_name".into(), function_name.clone())].into(),
    );
    match generate_entry_point_wrapper(
        db,
        item_function,
        wrapped_function_path,
        wrapper_function_name.clone(),
        generic_params,
        unsafe_new_contract_state_prefix,
    ) {
        Ok(generated_function) => {
            data.generated_wrapper_functions.push(generated_function);
            data.generated_wrapper_functions.push(RewriteNode::text("\n"));
            let generated = match entry_point_kind {
                EntryPointKind::Constructor => &mut data.constructor_functions,
                EntryPointKind::L1Handler => {
                    validate_l1_handler_first_parameter(db, &params, diagnostics);
                    &mut data.l1_handler_functions
                }
                EntryPointKind::External => &mut data.external_functions,
            };
            generated.push(RewriteNode::interpolate_patched(
                "\n    pub use super::$wrapper_function_name$ as $function_name$;",
                &[
                    ("wrapper_function_name".into(), wrapper_function_name),
                    ("function_name".into(), function_name),
                ]
                .into(),
            ));
        }
        Err(entry_point_diagnostics) => {
            diagnostics.extend(entry_point_diagnostics);
        }
    }
}

/// Generates Cairo code for an entry point wrapper.
fn generate_entry_point_wrapper(
    db: &dyn SyntaxGroup,
    function: &FunctionWithBody,
    wrapped_function_path: RewriteNode,
    wrapper_function_name: RewriteNode,
    generic_params: RewriteNode,
    unsafe_new_contract_state_prefix: &str,
) -> Result<RewriteNode, Vec<PluginDiagnostic>> {
    let declaration = function.declaration(db);
    let sig = declaration.signature(db);
    let mut params = sig.parameters(db).elements(db).into_iter().enumerate();
    let mut diagnostics = vec![];
    let mut arg_names = Vec::new();
    let mut arg_definitions = Vec::new();
    let mut ref_appends = Vec::new();

    let Some((0, first_param)) = params.next() else {
        return Err(vec![PluginDiagnostic::error(
            sig.stable_ptr().untyped(),
            "The first parameter of an entry point must be `self`.".into(),
        )]);
    };
    if first_param.name(db).text(db) != "self" {
        return Err(vec![PluginDiagnostic::error(
            first_param.stable_ptr().untyped(),
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
        let arg_name = format!("__arg_{}", param.name(db).text(db));
        let arg_type_ast =
            extract_matches!(param.type_clause(db), OptionTypeClause::TypeClause).ty(db);
        let type_name = arg_type_ast.as_syntax_node().get_text_without_trivia(db);

        let is_ref = param.is_ref_param(db);
        if raw_output && is_ref {
            diagnostics.push(PluginDiagnostic::error(
                param.modifiers(db).stable_ptr().untyped(),
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
            ("", "".to_string(), false, type_clause_ast.stable_ptr().untyped())
        }
        OptionReturnTypeClause::ReturnTypeClause(ty) => {
            let ret_type_ast = ty.ty(db);

            let return_ty_is_felt252_span = ret_type_ast.is_felt252_span(db);
            let ret_type_name = ret_type_ast.as_syntax_node().get_text_without_trivia(db);
            (
                "let res = ",
                format!("\n    core::serde::Serde::<{ret_type_name}>::serialize(@res, ref arr);"),
                return_ty_is_felt252_span,
                ret_type_ast.stable_ptr().untyped(),
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
                core::option::OptionTraitImpl::expect(core::gas::withdraw_gas(), 'Out of gas');
                $arg_definitions$
                assert(core::array::SpanTrait::is_empty(data), 'Input too long for arguments');
                core::option::OptionTraitImpl::expect(
                    core::gas::withdraw_gas_all(core::gas::get_builtin_costs()), 'Out of gas',
                );
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
    ).mapped(db, function))
}

/// Validates the first parameter of an L1 handler is `from_address: felt252` or `_from_address:
/// felt252`.
fn validate_l1_handler_first_parameter(
    db: &dyn SyntaxGroup,
    params: &ast::ParamList,
    diagnostics: &mut Vec<PluginDiagnostic>,
) {
    if let Some(first_param) = params.elements(db).get(1) {
        // Validate type

        if !extract_matches!(first_param.type_clause(db), OptionTypeClause::TypeClause)
            .ty(db)
            .is_felt252(db)
        {
            diagnostics.push(PluginDiagnostic::error(
                first_param.stable_ptr().untyped(),
                "The second parameter of an L1 handler must be of type `felt252`.".to_string(),
            ));
        }

        // Validate name
        if maybe_strip_underscore(first_param.name(db).text(db).as_str())
            != L1_HANDLER_FIRST_PARAM_NAME
        {
            diagnostics.push(PluginDiagnostic::error(
                first_param.stable_ptr().untyped(),
                "The second parameter of an L1 handler must be named 'from_address'.".to_string(),
            ));
        }
    } else {
        diagnostics.push(PluginDiagnostic::error(
            params.stable_ptr().untyped(),
            "An L1 handler must have the 'from_address' as its second parameter.".to_string(),
        ));
    };
}
