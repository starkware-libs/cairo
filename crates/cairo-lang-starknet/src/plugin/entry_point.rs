use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::ast::{
    self, Attribute, FunctionWithBody, OptionArgListParenthesized, OptionReturnTypeClause,
    OptionWrappedGenericParamList,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};
use indoc::{formatdoc, indoc};
use itertools::Itertools;

use super::consts::{
    CONSTRUCTOR_ATTR, CONSTRUCTOR_NAME, EXTERNAL_ATTR, IMPLICIT_PRECEDENCE, INCLUDE_ATTR,
    L1_HANDLER_ATTR, L1_HANDLER_FIRST_PARAM_NAME, RAW_OUTPUT_ATTR, WRAPPER_PREFIX,
};
use super::utils::{
    is_felt252, is_felt252_span, is_mut_param, is_ref_param, maybe_strip_underscore,
};

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
        if has_external_attribute(db, diagnostics, &ast::Item::FreeFunction(item_function.clone()))
        {
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
    pub fn try_from_attrs(db: &dyn SyntaxGroup, attrs: &impl QueryAttrs) -> Option<Self> {
        if attrs.has_attr(db, EXTERNAL_ATTR) {
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

/// Parameters for generating an entry point, used when calling `handle_entry_point`.
pub struct EntryPointGenerationParams<'a> {
    pub entry_point_kind: EntryPointKind,
    pub item_function: &'a FunctionWithBody,
    pub wrapped_function_path: RewriteNode,
    pub unsafe_new_contract_state: RewriteNode,
    pub generic_params: RewriteNode,
}

/// Handles a contract entrypoint function.
pub fn handle_entry_point(
    db: &dyn SyntaxGroup,
    EntryPointGenerationParams {
        entry_point_kind,
        item_function,
        wrapped_function_path,
        unsafe_new_contract_state,
        generic_params,
    }: EntryPointGenerationParams<'_>,
    diagnostics: &mut Vec<PluginDiagnostic>,
    data: &mut EntryPointsGenerationData,
) {
    let declaration = item_function.declaration(db);
    let name_node = declaration.name(db);
    if entry_point_kind == EntryPointKind::Constructor && name_node.text(db) != CONSTRUCTOR_NAME {
        diagnostics.push(PluginDiagnostic {
            message: format!("The constructor function must be called `{CONSTRUCTOR_NAME}`."),
            stable_ptr: name_node.stable_ptr().untyped(),
        });
    }

    if let OptionWrappedGenericParamList::WrappedGenericParamList(generic_params) =
        declaration.generic_params(db)
    {
        diagnostics.push(PluginDiagnostic {
            message: "Contract entry points cannot have generic arguments".to_string(),
            stable_ptr: generic_params.stable_ptr().untyped(),
        })
    }

    let mut declaration_node = RewriteNode::new_trimmed(declaration.as_syntax_node());
    let original_parameters = declaration_node
        .modify_child(db, ast::FunctionDeclaration::INDEX_SIGNATURE)
        .modify_child(db, ast::FunctionSignature::INDEX_PARAMETERS);
    let params = declaration.signature(db).parameters(db);
    for (param_idx, param) in params.elements(db).iter().enumerate() {
        // This assumes `mut` can only appear alone.
        if is_mut_param(db, param) {
            original_parameters
                .modify_child(db, param_idx * 2)
                .modify_child(db, ast::Param::INDEX_MODIFIERS)
                .set_str("".to_string());
        }
    }
    let function_name = RewriteNode::new_trimmed(name_node.as_syntax_node());
    let wrapper_function_name = RewriteNode::interpolate_patched(
        format!("{WRAPPER_PREFIX}$function_name$").as_str(),
        [("function_name".into(), function_name.clone())].into(),
    );
    match generate_entry_point_wrapper(
        db,
        item_function,
        wrapped_function_path,
        wrapper_function_name.clone(),
        generic_params,
        unsafe_new_contract_state,
    ) {
        Ok(generated_function) => {
            data.generated_wrapper_functions.push(generated_function);
            data.generated_wrapper_functions.push(RewriteNode::Text("\n".to_string()));
            let generated = match entry_point_kind {
                EntryPointKind::Constructor => &mut data.constructor_functions,
                EntryPointKind::L1Handler => {
                    validate_l1_handler_first_parameter(db, &params, diagnostics);
                    &mut data.l1_handler_functions
                }
                EntryPointKind::External => &mut data.external_functions,
            };
            generated.push(RewriteNode::interpolate_patched(
                "\n        use super::$wrapper_function_name$ as $function_name$;",
                [
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
    unsafe_new_contract_state: RewriteNode,
) -> Result<RewriteNode, Vec<PluginDiagnostic>> {
    let declaration = function.declaration(db);
    let sig = declaration.signature(db);
    let mut params = sig.parameters(db).elements(db).into_iter().enumerate();
    let mut diagnostics = vec![];
    let mut arg_names = Vec::new();
    let mut arg_definitions = Vec::new();
    let mut ref_appends = Vec::new();

    let Some((0, first_param)) = params.next() else {
        return Err(vec![PluginDiagnostic {
            message: "The first parameter of an entry point must be `self`.".into(),
            stable_ptr: sig.stable_ptr().untyped(),
        }]);
    };
    if first_param.name(db).text(db) != "self" {
        return Err(vec![PluginDiagnostic {
            message: "The first parameter of an entry point must be `self`.".into(),
            stable_ptr: first_param.stable_ptr().untyped(),
        }]);
    };
    let is_snapshot = matches!(first_param.type_clause(db).ty(db), ast::Expr::Unary(_));
    // TODO(spapini): Check modifiers and type.

    let raw_output = function.has_attr(db, RAW_OUTPUT_ATTR);
    for (param_idx, param) in params {
        let arg_name = format!("__arg_{}", param.name(db).text(db));
        let arg_type_ast = param.type_clause(db).ty(db);
        let type_name = arg_type_ast.as_syntax_node().get_text_without_trivia(db);

        let is_ref = is_ref_param(db, &param);
        if raw_output && is_ref {
            diagnostics.push(PluginDiagnostic {
                message: format!("`{RAW_OUTPUT_ATTR}` functions cannot have `ref` parameters."),
                stable_ptr: param.modifiers(db).stable_ptr().untyped(),
            });
        }
        let ref_modifier = if is_ref { "ref " } else { "" };
        arg_names.push(format!("{ref_modifier}{arg_name}"));
        let mut_modifier = if is_ref { "mut " } else { "" };
        let arg_definition = formatdoc!(
            "
            let {mut_modifier}{arg_name} = option::OptionTraitImpl::expect(
                    serde::Serde::<{type_name}>::deserialize(ref data),
                    'Failed to deserialize param #{param_idx}'
                );"
        );
        arg_definitions.push(arg_definition);

        if is_ref {
            ref_appends.push(RewriteNode::Text(format!(
                "\n            serde::Serde::<{type_name}>::serialize(@{arg_name}, ref arr);"
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

            let return_ty_is_felt252_span = is_felt252_span(db, &ret_type_ast);
            let ret_type_name = ret_type_ast.as_syntax_node().get_text_without_trivia(db);
            (
                "let res = ",
                format!("\n    serde::Serde::<{ret_type_name}>::serialize(@res, ref arr);"),
                return_ty_is_felt252_span,
                ret_type_ast.stable_ptr().untyped(),
            )
        }
    };

    if raw_output && !return_ty_is_felt252_span {
        diagnostics.push(PluginDiagnostic {
            message: format!("`{RAW_OUTPUT_ATTR}` functions must return `Span::<felt252>`."),
            stable_ptr: ret_type_ptr,
        });
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
                let mut arr = array::array_new();
                // References.$ref_appends$
                // Result.{append_res}
                array::ArrayTrait::span(@arr)"
        }
    };

    let output_handling = RewriteNode::interpolate_patched(
        &output_handling_string,
        [
            ("wrapped_function_path".to_string(), wrapped_function_path),
            ("ref_appends".to_string(), RewriteNode::new_modified(ref_appends)),
        ]
        .into(),
    );

    let implicit_precedence = RewriteNode::Text(format!("#[implicit_precedence({})]", {
        IMPLICIT_PRECEDENCE.iter().join(", ")
    }));

    let arg_definitions = RewriteNode::Text(arg_definitions.join("\n    "));

    Ok(RewriteNode::interpolate_patched(
        indoc! {"
            $implicit_precedence$
            fn $wrapper_function_name$$generic_params$(mut data: Span::<felt252>) -> Span::<felt252> {
                internal::require_implicit::<System>();
                internal::revoke_ap_tracking();
                option::OptionTraitImpl::expect(gas::withdraw_gas(), 'Out of gas');
                $arg_definitions$
                assert(array::SpanTrait::is_empty(data), 'Input too long for arguments');
                option::OptionTraitImpl::expect(
                    gas::withdraw_gas_all(get_builtin_costs()), 'Out of gas',
                );
                let mut contract_state = $unsafe_new_contract_state$;
                $output_handling$
            }
        "},
        [
            ("wrapper_function_name".to_string(), wrapper_function_name),
            ("generic_params".to_string(), generic_params),
            ("unsafe_new_contract_state".to_string(), unsafe_new_contract_state),
            ("output_handling".to_string(), output_handling),
            ("arg_definitions".to_string(), arg_definitions),
            ("implicit_precedence".to_string(), implicit_precedence),
        ]
        .into(),
    ))
}

/// Checks if the item is marked with an external attribute. Also validates the attribute.
pub fn has_external_attribute(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
) -> bool {
    let Some(attr) = item.find_attr(db, EXTERNAL_ATTR) else {
        return false;
    };
    validate_v0(db, diagnostics, &attr, EXTERNAL_ATTR);
    true
}

/// Checks if the item is marked with an include attribute. Also validates the attribute.
pub fn has_include_attribute(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
) -> bool {
    let Some(attr) = item.find_attr(db, INCLUDE_ATTR) else {
        return false;
    };
    validate_v0(db, diagnostics, &attr, INCLUDE_ATTR);
    true
}

/// Assuming the attribute is `name`, validate it's #[`name`(v0)].
fn validate_v0(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    attr: &Attribute,
    name: &str,
) {
    if !is_arg_v0(db, attr) {
        diagnostics.push(PluginDiagnostic {
            message: format!("Only #[{name}(v0)] is supported."),
            stable_ptr: attr.stable_ptr().untyped(),
        });
    }
}

/// Checks if the only arg of the given attribute is "v0".
fn is_arg_v0(db: &dyn SyntaxGroup, attr: &Attribute) -> bool {
    match attr.arguments(db) {
        OptionArgListParenthesized::ArgListParenthesized(y) => {
            matches!(&y.args(db).elements(db)[..],
            [arg] if arg.as_syntax_node().get_text_without_trivia(db) == "v0")
        }
        OptionArgListParenthesized::Empty(_) => false,
    }
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
        if !is_felt252(db, &first_param.type_clause(db).ty(db)) {
            diagnostics.push(PluginDiagnostic {
                message: "The second parameter of an L1 handler must be of type `felt252`."
                    .to_string(),
                stable_ptr: first_param.stable_ptr().untyped(),
            });
        }

        // Validate name
        if maybe_strip_underscore(first_param.name(db).text(db).as_str())
            != L1_HANDLER_FIRST_PARAM_NAME
        {
            diagnostics.push(PluginDiagnostic {
                message: "The second parameter of an L1 handler must be named 'from_address'."
                    .to_string(),
                stable_ptr: first_param.stable_ptr().untyped(),
            });
        }
    } else {
        diagnostics.push(PluginDiagnostic {
            message: "An L1 handler must have the 'from_address' as its second parameter."
                .to_string(),
            stable_ptr: params.stable_ptr().untyped(),
        });
    };
}
