use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::ast::OptionWrappedGenericParamList;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use indoc::formatdoc;

use super::generation_data::{ContractGenerationData, StarknetModuleCommonGenerationData};
use super::{generate_submodule, StarknetModuleKind};
use crate::contract::starknet_keccak;
use crate::plugin::consts::{
    CONSTRUCTOR_ATTR, CONSTRUCTOR_MODULE, CONSTRUCTOR_NAME, EXTERNAL_ATTR, EXTERNAL_MODULE,
    L1_HANDLER_ATTR, L1_HANDLER_FIRST_PARAM_NAME, L1_HANDLER_MODULE, STORAGE_STRUCT_NAME,
    WRAPPER_PREFIX,
};
use crate::plugin::entry_point::{
    generate_entry_point_wrapper, has_external_attribute, has_include_attribute, EntryPointKind,
};
use crate::plugin::storage::handle_storage_struct;
use crate::plugin::utils::{is_felt252, is_mut_param, maybe_strip_underscore};

/// Accumulated data specific for contract generation.
#[derive(Default)]
pub struct ContractSpecificGenerationData {
    generated_wrapper_functions: Vec<RewriteNode>,
    external_functions: Vec<RewriteNode>,
    constructor_functions: Vec<RewriteNode>,
    l1_handler_functions: Vec<RewriteNode>,
    test_config: RewriteNode,
}
impl ContractSpecificGenerationData {
    pub fn to_rewrite_node(self) -> RewriteNode {
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
            "$test_config$
$generated_wrapper_functions$
    $generated_external_module$
    $generated_l1_handler_module$
    $generated_constructor_module$",
            [
                ("test_config".to_string(), self.test_config),
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

fn handle_contract_item(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
    data: &mut ContractGenerationData,
) {
    match &item {
        ast::Item::FreeFunction(item_function) => {
            handle_contract_free_function(db, diagnostics, item_function, &mut data.specific);
        }
        ast::Item::Impl(item_impl) => {
            handle_contract_impl(db, diagnostics, &item, item_impl, &mut data.specific);
        }
        ast::Item::Struct(item_struct) if item_struct.name(db).text(db) == STORAGE_STRUCT_NAME => {
            handle_storage_struct(
                db,
                diagnostics,
                item_struct.clone(),
                StarknetModuleKind::Contract,
                &mut data.common,
            );
        }
        _ => {}
    }
}

pub(super) fn generate_contract_specific_code(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    common_data: StarknetModuleCommonGenerationData,
    body: &ast::ModuleBody,
    module_ast: &ast::ItemModule,
) -> RewriteNode {
    let mut generation_data = ContractGenerationData { common: common_data, ..Default::default() };
    for item in body.items(db).elements(db) {
        handle_contract_item(db, diagnostics, &item, &mut generation_data);
    }

    let test_class_hash = format!(
        "0x{:x}",
        starknet_keccak(
            module_ast.as_syntax_node().get_text_without_trivia(db).as_str().as_bytes(),
        )
    );

    generation_data.specific.test_config = RewriteNode::Text(formatdoc!(
        "#[cfg(test)]
            const TEST_CLASS_HASH: felt252 = {test_class_hash};
"
    ));

    generation_data.to_rewrite_node()
}

/// Forbids the given attribute in the given impl, assuming it's external.
fn forbid_attribute_in_external_impl(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    impl_item: &ast::ImplItem,
    attr_name: &str,
) {
    if let Some(attr) = impl_item.find_attr(db, attr_name) {
        diagnostics.push(PluginDiagnostic {
            message: format!("The '{attr_name}' attribute is not allowed inside an external impl."),
            stable_ptr: attr.stable_ptr().untyped(),
        });
    }
}

/// Handles a contract entrypoint function.
fn handle_contract_entry_point(
    entry_point_kind: EntryPointKind,
    item_function: &ast::FunctionWithBody,
    wrapped_function_path: RewriteNode,
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    data: &mut ContractSpecificGenerationData,
) {
    let name_node = item_function.declaration(db).name(db);
    if entry_point_kind == EntryPointKind::Constructor && name_node.text(db) != CONSTRUCTOR_NAME {
        diagnostics.push(PluginDiagnostic {
            message: format!("The constructor function must be called `{CONSTRUCTOR_NAME}`."),
            stable_ptr: name_node.stable_ptr().untyped(),
        });
    }

    let declaration = item_function.declaration(db);
    if let OptionWrappedGenericParamList::WrappedGenericParamList(generic_params) =
        declaration.generic_params(db)
    {
        diagnostics.push(PluginDiagnostic {
            message: "Contract entry points cannot have generic arguments".to_string(),
            stable_ptr: generic_params.stable_ptr().untyped(),
        })
    }

    // TODO(ilya): Validate that an account contract has all the required functions.

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

/// Handles a free function inside a contract module.
fn handle_contract_free_function(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item_function: &ast::FunctionWithBody,
    data: &mut ContractSpecificGenerationData,
) {
    let Some(entry_point_kind) =
        EntryPointKind::try_from_function_with_body(db, diagnostics, item_function)
    else {
        return;
    };
    let function_name =
        RewriteNode::new_trimmed(item_function.declaration(db).name(db).as_syntax_node());
    handle_contract_entry_point(
        entry_point_kind,
        item_function,
        function_name,
        db,
        diagnostics,
        data,
    );
}

/// Handles an impl inside a contract module.
fn handle_contract_impl(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
    item_impl: &ast::ItemImpl,
    data: &mut ContractSpecificGenerationData,
) {
    let is_external = has_external_attribute(db, diagnostics, item);
    if !(is_external || has_include_attribute(db, diagnostics, item)) {
        return;
    }
    let ast::MaybeImplBody::Some(impl_body) = item_impl.body(db) else {
        return;
    };
    let impl_name = RewriteNode::new_trimmed(item_impl.name(db).as_syntax_node());
    for item in impl_body.items(db).elements(db) {
        if is_external {
            for attr in [EXTERNAL_ATTR, CONSTRUCTOR_ATTR, L1_HANDLER_ATTR] {
                forbid_attribute_in_external_impl(db, diagnostics, &item, attr);
            }
        }

        let ast::ImplItem::Function(item_function) = item else {
            continue;
        };
        let entry_point_kind = if is_external || item_function.has_attr(db, EXTERNAL_ATTR) {
            EntryPointKind::External
        } else if item_function.has_attr(db, CONSTRUCTOR_ATTR) {
            EntryPointKind::Constructor
        } else if item_function.has_attr(db, L1_HANDLER_ATTR) {
            EntryPointKind::L1Handler
        } else {
            continue;
        };
        let function_name =
            RewriteNode::new_trimmed(item_function.declaration(db).name(db).as_syntax_node());
        let function_name = RewriteNode::interpolate_patched(
            "$impl_name$::$func_name$",
            [
                ("impl_name".to_string(), impl_name.clone()),
                ("func_name".to_string(), function_name),
            ]
            .into(),
        );
        handle_contract_entry_point(
            entry_point_kind,
            &item_function,
            function_name,
            db,
            diagnostics,
            data,
        );
    }
}
