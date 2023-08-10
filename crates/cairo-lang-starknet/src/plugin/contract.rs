use std::vec;

use cairo_lang_defs::db::get_all_path_leafs;
use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::node::ast::{MaybeModuleBody, OptionWrappedGenericParamList};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, QueryAttrs};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use indoc::formatdoc;

use super::consts::{
    CONSTRUCTOR_ATTR, CONSTRUCTOR_MODULE, CONSTRUCTOR_NAME, CONTRACT_ATTR,
    DEPRECATED_CONTRACT_ATTR, EVENT_ATTR, EVENT_TYPE_NAME, EXTERNAL_ATTR, EXTERNAL_MODULE,
    L1_HANDLER_ATTR, L1_HANDLER_FIRST_PARAM_NAME, L1_HANDLER_MODULE, STORAGE_ATTR,
    STORAGE_STRUCT_NAME, WRAPPER_PREFIX,
};
use super::entry_point::{
    generate_entry_point_wrapper, has_external_attribute, has_include_attribute, EntryPointKind,
};
use super::storage::handle_storage_struct;
use super::utils::{is_felt252, is_mut_param, maybe_strip_underscore};
use crate::contract::starknet_keccak;
use crate::plugin::aux_data::StarkNetContractAuxData;

/// Handles a contract module item.
pub fn handle_module(db: &dyn SyntaxGroup, module_ast: ast::ItemModule) -> PluginResult {
    if module_ast.has_attr(db, DEPRECATED_CONTRACT_ATTR) {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: format!(
                    "The '{DEPRECATED_CONTRACT_ATTR}' attribute was deprecated, please use \
                     `{CONTRACT_ATTR}` instead.",
                ),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    }
    if !module_ast.has_attr(db, CONTRACT_ATTR) {
        return PluginResult::default();
    }

    let MaybeModuleBody::Some(body) = module_ast.body(db) else {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: "Contracts without body are not supported.".to_string(),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    };
    let Some(storage_struct_ast) = body.items(db).elements(db).into_iter().find(|item| {
        matches!(item, ast::Item::Struct(struct_ast) if struct_ast.name(db).text(db) == STORAGE_STRUCT_NAME)
    }) else {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: "Contracts must define a 'Storage' struct.".to_string(),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    };

    if !storage_struct_ast.has_attr(db, STORAGE_ATTR) {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: "'Storage' struct must be annotated with #[storage].".to_string(),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    }

    PluginResult::default()
}

/// Accumulated data for contract generation.
#[derive(Default)]
struct ContractGenerationData {
    generated_wrapper_functions: Vec<RewriteNode>,
    external_functions: Vec<RewriteNode>,
    constructor_functions: Vec<RewriteNode>,
    l1_handler_functions: Vec<RewriteNode>,
}

/// If the module is annotated with CONTRACT_ATTR, generate the relevant contract logic.
pub fn handle_contract_by_storage(
    db: &dyn SyntaxGroup,
    struct_ast: ast::ItemStruct,
) -> Option<PluginResult> {
    let module_node = struct_ast.as_syntax_node().parent()?.parent()?.parent()?;
    if module_node.kind(db) != SyntaxKind::ItemModule {
        return None;
    }
    let module_ast = ast::ItemModule::from_syntax_node(db, module_node);

    if !module_ast.has_attr(db, CONTRACT_ATTR) {
        return None;
    }

    let body = extract_matches!(module_ast.body(db), MaybeModuleBody::Some);
    let mut diagnostics = vec![];

    // Use declarations to add to the internal submodules. Mapping from 'use' items to their path.
    let mut extra_uses = OrderedHashMap::default();
    // Whether an event exists in the given contract module. If it doesn't, we need to generate an
    // empty one.
    let mut has_event = false;
    for item in body.items(db).elements(db) {
        // Skip elements that only generate other code, but their code itself is ignored.
        if matches!(&item, ast::Item::Struct(item) if item.name(db).text(db) == STORAGE_STRUCT_NAME)
        {
            continue;
        }

        if is_starknet_event(db, &mut diagnostics, &item) {
            has_event = true;
        }

        maybe_add_extra_use(db, item, &mut extra_uses);
    }

    let extra_uses_node = RewriteNode::new_modified(
        extra_uses
            .values()
            .map(|use_path| RewriteNode::Text(format!("\n        use {use_path};")))
            .collect(),
    );

    let mut data = ContractGenerationData::default();

    // Generate the code for ContractState and the entry points.
    let contract_state_code = generate_entry_points_and_contract_state(
        db,
        &mut diagnostics,
        body,
        &mut data,
        &extra_uses_node,
        has_event,
    );

    let module_name_ast = module_ast.name(db);
    let test_class_hash = format!(
        "0x{:x}",
        starknet_keccak(
            module_ast.as_syntax_node().get_text_without_trivia(db).as_str().as_bytes(),
        )
    );
    let generated_contract_mod = RewriteNode::interpolate_patched(
        formatdoc!(
            "
            #[cfg(test)]
            const TEST_CLASS_HASH: felt252 = {test_class_hash};
            $contract_state_code$

            $generated_wrapper_functions$

            mod {EXTERNAL_MODULE} {{$external_functions$
            }}

            mod {L1_HANDLER_MODULE} {{$l1_handler_functions$
            }}

            mod {CONSTRUCTOR_MODULE} {{$constructor_functions$
            }}
        "
        )
        .as_str(),
        [
            ("contract_state_code".to_string(), contract_state_code),
            (
                "generated_wrapper_functions".to_string(),
                RewriteNode::new_modified(data.generated_wrapper_functions),
            ),
            ("external_functions".to_string(), RewriteNode::new_modified(data.external_functions)),
            (
                "l1_handler_functions".to_string(),
                RewriteNode::new_modified(data.l1_handler_functions),
            ),
            (
                "constructor_functions".to_string(),
                RewriteNode::new_modified(data.constructor_functions),
            ),
        ]
        .into(),
    );

    let mut builder = PatchBuilder::new(db);
    builder.add_modified(generated_contract_mod);
    Some(PluginResult {
        code: Some(PluginGeneratedFile {
            name: "contract".into(),
            content: builder.code,
            patches: builder.patches,
            aux_data: Some(DynGeneratedFileAuxData::new(StarkNetContractAuxData {
                contracts: vec![module_name_ast.text(db)],
            })),
        }),
        diagnostics,
        remove_original_item: true,
    })
}

/// Generates the code for the entry points of the contract, and for the ContractState.
fn generate_entry_points_and_contract_state(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    body: ast::ModuleBody,
    data: &mut ContractGenerationData,
    extra_uses_node: &RewriteNode,
    has_event: bool,
) -> RewriteNode {
    let mut contract_state_code = RewriteNode::Text("".to_string());
    for item in body.items(db).elements(db) {
        match &item {
            ast::Item::FreeFunction(item_function) => {
                let Some(entry_point_kind) =
                    EntryPointKind::try_from_function_with_body(db, diagnostics, item_function)
                else {
                    continue;
                };
                let function_name = RewriteNode::new_trimmed(
                    item_function.declaration(db).name(db).as_syntax_node(),
                );
                handle_entry_point(
                    entry_point_kind,
                    item_function,
                    function_name,
                    db,
                    diagnostics,
                    data,
                );
            }
            ast::Item::Impl(item_impl) => {
                let is_external = has_external_attribute(db, diagnostics, &item);
                if !(is_external || has_include_attribute(db, diagnostics, &item)) {
                    continue;
                }
                let ast::MaybeImplBody::Some(impl_body) = item_impl.body(db) else {
                    continue;
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
                    let entry_point_kind =
                        if is_external || item_function.has_attr(db, EXTERNAL_ATTR) {
                            EntryPointKind::External
                        } else if item_function.has_attr(db, CONSTRUCTOR_ATTR) {
                            EntryPointKind::Constructor
                        } else if item_function.has_attr(db, L1_HANDLER_ATTR) {
                            EntryPointKind::L1Handler
                        } else {
                            continue;
                        };
                    let function_name = RewriteNode::new_trimmed(
                        item_function.declaration(db).name(db).as_syntax_node(),
                    );
                    let function_name = RewriteNode::interpolate_patched(
                        "$impl_name$::$func_name$",
                        [
                            ("impl_name".to_string(), impl_name.clone()),
                            ("func_name".to_string(), function_name),
                        ]
                        .into(),
                    );
                    handle_entry_point(
                        entry_point_kind,
                        &item_function,
                        function_name,
                        db,
                        diagnostics,
                        data,
                    );
                }
            }
            ast::Item::Struct(item_struct)
                if item_struct.name(db).text(db) == STORAGE_STRUCT_NAME =>
            {
                let (contract_state_rewrite_node, storage_diagnostics) =
                    handle_storage_struct(db, item_struct.clone(), extra_uses_node, has_event);
                contract_state_code = contract_state_rewrite_node;
                diagnostics.extend(storage_diagnostics);
            }
            _ => {}
        }
    }
    contract_state_code
}

/// Adds extra uses, to be used in the generated submodules.
fn maybe_add_extra_use(
    db: &dyn SyntaxGroup,
    item: ast::Item,
    extra_uses: &mut OrderedHashMap<smol_str::SmolStr, String>,
) {
    if let Some(ident) = match item {
        ast::Item::Use(item) => {
            let leaves = get_all_path_leafs(db, item.use_path(db));
            for leaf in leaves {
                extra_uses
                    .entry(leaf.stable_ptr().identifier(db))
                    .or_insert_with_key(|ident| format!("super::{}", ident));
            }
            None
        }
        ast::Item::Constant(item) => Some(item.name(db)),
        ast::Item::Module(item) => Some(item.name(db)),
        ast::Item::Impl(item) => Some(item.name(db)),
        ast::Item::Struct(item) => Some(item.name(db)),
        ast::Item::Enum(item) => Some(item.name(db)),
        ast::Item::TypeAlias(item) => Some(item.name(db)),
        // These items are not directly required in generated inner modules.
        ast::Item::ExternFunction(_)
        | ast::Item::ExternType(_)
        | ast::Item::Trait(_)
        | ast::Item::FreeFunction(_)
        | ast::Item::ImplAlias(_)
        | ast::Item::Missing(_) => None,
    } {
        extra_uses.entry(ident.text(db)).or_insert_with_key(|ident| format!("super::{}", ident));
    }
}

/// Checks whether the given item is an event, and if so - makes sure it's valid.
fn is_starknet_event(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
) -> bool {
    let (has_event_name, stable_ptr) = match item {
        ast::Item::Struct(strct) => {
            (strct.name(db).text(db) == EVENT_TYPE_NAME, strct.name(db).stable_ptr().untyped())
        }
        ast::Item::Enum(enm) => {
            (enm.name(db).text(db) == EVENT_TYPE_NAME, enm.name(db).stable_ptr().untyped())
        }
        ast::Item::Use(item) => {
            for leaf in get_all_path_leafs(db, item.use_path(db)) {
                let stable_ptr = &leaf.stable_ptr();
                if stable_ptr.identifier(db) == EVENT_TYPE_NAME {
                    if !item.has_attr(db, EVENT_ATTR) {
                        diagnostics.push(PluginDiagnostic {
                            message: format!(
                                "Contract type that is named `Event` must be marked with \
                                 #[{EVENT_ATTR}]."
                            ),
                            stable_ptr: stable_ptr.untyped(),
                        });
                    }
                    return true;
                }
            }
            return false;
        }
        _ => return false,
    };
    let has_event_attr = item.has_attr(db, EVENT_ATTR);

    match (has_event_attr, has_event_name) {
        (true, false) => {
            diagnostics.push(PluginDiagnostic {
                message: format!(
                    "Contract type that is marked with #[{EVENT_ATTR}] must be named `Event`."
                ),
                stable_ptr,
            });
            false
        }
        (false, true) => {
            diagnostics.push(PluginDiagnostic {
                message: format!(
                    "Contract type that is named `Event` must be marked with #[{EVENT_ATTR}]."
                ),
                stable_ptr,
            });
            // The attribute is missing, but this counts as a event - we can't create another
            // (empty) event.
            true
        }
        (true, true) => true,
        (false, false) => false,
    }
}

fn forbid_attribute_in_external_impl(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    impl_item: &ast::ImplItem,
    attr_name: &str,
) {
    if let Some(attr) = impl_item.find_attr(db, attr_name) {
        diagnostics.push(PluginDiagnostic {
            message: format!(
                "The '{attr_name}' attribute is not allowed inside a contract external impl."
            ),
            stable_ptr: attr.stable_ptr().untyped(),
        });
    }
}

/// Handles a contract entrypoint function.
fn handle_entry_point(
    entry_point_kind: EntryPointKind,
    item_function: &ast::FunctionWithBody,
    wrapped_function_path: RewriteNode,
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    data: &mut ContractGenerationData,
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
                "\n    use super::$wrapper_function_name$ as $function_name$;",
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
