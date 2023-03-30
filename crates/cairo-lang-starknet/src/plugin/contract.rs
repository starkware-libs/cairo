use std::collections::HashMap;
use std::vec;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_semantic::plugin::DynPluginAuxData;
use cairo_lang_syntax::node::ast::{MaybeModuleBody, OptionWrappedGenericParamList};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use indoc::formatdoc;

use super::consts::{
    ABI_TRAIT, ACCOUNT_CONTRACT_ATTR, ACCOUNT_CONTRACT_ENTRY_POINTS, CONSTRUCTOR_MODULE,
    CONTRACT_ATTR, EVENT_ATTR, EXTERNAL_MODULE, L1_HANDLER_FIRST_PARAM_NAME, L1_HANDLER_MODULE,
    STORAGE_STRUCT_NAME,
};
use super::entry_point::{generate_entry_point_wrapper, EntryPointKind};
use super::events::handle_event;
use super::storage::handle_storage_struct;
use super::utils::{is_felt252, is_mut_param, maybe_strip_underscore};
use crate::plugin::aux_data::StarkNetContractAuxData;

/// If the module is annotated with CONTRACT_ATTR, generate the relevant contract logic.
pub fn handle_mod(db: &dyn SyntaxGroup, module_ast: ast::ItemModule) -> PluginResult {
    let is_account_contract = module_ast.has_attr(db, ACCOUNT_CONTRACT_ATTR);

    if !is_account_contract && !module_ast.has_attr(db, CONTRACT_ATTR) {
        return PluginResult::default();
    }

    let body = match module_ast.body(db) {
        MaybeModuleBody::Some(body) => body,
        MaybeModuleBody::None(empty_body) => {
            return PluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic {
                    message: "Contracts without body are not supported.".to_string(),
                    stable_ptr: empty_body.stable_ptr().untyped(),
                }],
                remove_original_item: false,
            };
        }
    };
    let mut diagnostics = vec![];
    let mut kept_original_items = Vec::new();

    // A maping from a 'use' item to its path.
    let mut extra_uses = OrderedHashMap::default();
    for item in body.items(db).elements(db) {
        // Skipping elements that only generate other code, but their code itself is ignored.
        if matches!(&item, ast::Item::FreeFunction(item) if item.has_attr(db, EVENT_ATTR))
            || matches!(&item, ast::Item::Struct(item) if item.name(db).text(db) == STORAGE_STRUCT_NAME)
        {
            continue;
        }
        kept_original_items.push(RewriteNode::Copied(item.as_syntax_node()));
        if let Some(ident) = match item {
            ast::Item::Constant(item) => Some(item.name(db)),
            ast::Item::Module(item) => Some(item.name(db)),
            ast::Item::Use(item) => {
                if let Some(ast::PathSegment::Simple(final_section)) =
                    item.name(db).elements(db).last()
                {
                    Some(final_section.ident(db))
                } else {
                    None
                }
            }
            ast::Item::Impl(item) => Some(item.name(db)),
            ast::Item::Struct(item) => Some(item.name(db)),
            ast::Item::Enum(item) => Some(item.name(db)),
            ast::Item::TypeAlias(item) => Some(item.name(db)),
            // Externs, trait declarations and free functions are not directly required in generated
            // inner modules.
            ast::Item::ExternFunction(_)
            | ast::Item::ExternType(_)
            | ast::Item::Trait(_)
            | ast::Item::FreeFunction(_) => None,
        } {
            extra_uses
                .entry(ident.text(db))
                .or_insert_with_key(|ident| format!("super::{}", ident));
        }
    }

    for (use_item, path) in [
        ("ClassHashSerde", "starknet::class_hash::ClassHashSerde"),
        ("ContractAddressSerde", "starknet::contract_address::ContractAddressSerde"),
        ("StorageAddressSerde", "starknet::storage_access::StorageAddressSerde"),
    ]
    .into_iter()
    {
        extra_uses.entry(use_item.into()).or_insert_with(|| path.to_string());
    }

    let extra_uses_node = RewriteNode::new_modified(
        extra_uses
            .values()
            .map(|use_path| RewriteNode::Text(format!("\n        use {use_path};")))
            .collect(),
    );
    let mut generated_external_functions = Vec::new();
    let mut generated_constructor_functions = Vec::new();
    let mut generated_l1_handler_functions = Vec::new();

    let mut storage_code = RewriteNode::Text("".to_string());
    let mut abi_functions = Vec::new();
    let mut event_functions = Vec::new();
    let mut abi_events = Vec::new();
    for item in body.items(db).elements(db) {
        match &item {
            ast::Item::FreeFunction(item_function) if item_function.has_attr(db, EVENT_ATTR) => {
                let (rewrite_nodes, event_diagnostics) = handle_event(db, item_function.clone());
                if let Some((event_function_rewrite, abi_event_rewrite)) = rewrite_nodes {
                    event_functions.push(event_function_rewrite);
                    abi_events.push(abi_event_rewrite);
                }
                diagnostics.extend(event_diagnostics);
            }
            ast::Item::FreeFunction(item_function) => {
                if let Some(entry_point_kind) =
                    EntryPointKind::try_from_function_with_body(db, item_function)
                {
                    let attr = entry_point_kind.get_attr();

                    let declaration = item_function.declaration(db);
                    if let OptionWrappedGenericParamList::WrappedGenericParamList(generic_params) =
                        declaration.generic_params(db)
                    {
                        diagnostics.push(PluginDiagnostic {
                            message: "Contract entry points cannot have generic arguments"
                                .to_string(),
                            stable_ptr: generic_params.stable_ptr().untyped(),
                        })
                    }

                    let name = declaration.name(db);
                    let name_str = name.text(db);

                    if !is_account_contract {
                        for account_contract_entry_point in ACCOUNT_CONTRACT_ENTRY_POINTS {
                            if name_str == account_contract_entry_point {
                                diagnostics.push(PluginDiagnostic {
                                    message: format!(
                                        "Only an account contract may implement `{name_str}`."
                                    ),

                                    stable_ptr: name.stable_ptr().untyped(),
                                })
                            }
                        }
                    }
                    // TODO(ilya): Validate that an account contract has all the required functions.

                    let mut declaration_node =
                        RewriteNode::new_trimmed(declaration.as_syntax_node());
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
                    abi_functions.push(RewriteNode::new_modified(vec![
                        RewriteNode::Text(format!("#[{attr}]\n        ")),
                        declaration_node,
                        RewriteNode::Text(";\n        ".to_string()),
                    ]));

                    match generate_entry_point_wrapper(db, item_function) {
                        Ok(generated_function) => {
                            let generated = match entry_point_kind {
                                EntryPointKind::Constructor => &mut generated_constructor_functions,
                                EntryPointKind::L1Handler => {
                                    validate_l1_handler_first_parameter(
                                        db,
                                        &params,
                                        &mut diagnostics,
                                    );
                                    &mut generated_l1_handler_functions
                                }
                                EntryPointKind::External | EntryPointKind::View => {
                                    &mut generated_external_functions
                                }
                            };
                            generated.push(generated_function);
                            generated.push(RewriteNode::Text("\n        ".to_string()));
                        }
                        Err(entry_point_diagnostics) => {
                            diagnostics.extend(entry_point_diagnostics);
                        }
                    }
                }
            }
            ast::Item::Struct(item_struct)
                if item_struct.name(db).text(db) == STORAGE_STRUCT_NAME =>
            {
                let (storage_rewrite_node, storage_diagnostics) =
                    handle_storage_struct(db, item_struct.clone(), &extra_uses_node);
                storage_code = storage_rewrite_node;
                diagnostics.extend(storage_diagnostics);
            }
            _ => {}
        }
    }

    let module_name_ast = module_ast.name(db);
    let generated_contract_mod = RewriteNode::interpolate_patched(
        formatdoc!(
            "
            mod $contract_name$ {{
                use starknet::SyscallResultTrait;
                use starknet::SyscallResultTraitImpl;

            $original_items$
                $storage_code$

                $event_functions$

                trait {ABI_TRAIT} {{
                    $abi_functions$
                    $abi_events$
                }}

                mod {EXTERNAL_MODULE} {{$extra_uses$

                    $generated_external_functions$
                }}

                mod {L1_HANDLER_MODULE} {{$extra_uses$

                    $generated_l1_handler_functions$
                }}

                mod {CONSTRUCTOR_MODULE} {{$extra_uses$

                    $generated_constructor_functions$
                }}
            }}
        "
        )
        .as_str(),
        HashMap::from([
            (
                "contract_name".to_string(),
                RewriteNode::new_trimmed(module_name_ast.as_syntax_node()),
            ),
            ("original_items".to_string(), RewriteNode::new_modified(kept_original_items)),
            ("storage_code".to_string(), storage_code),
            ("event_functions".to_string(), RewriteNode::new_modified(event_functions)),
            ("abi_functions".to_string(), RewriteNode::new_modified(abi_functions)),
            ("abi_events".to_string(), RewriteNode::new_modified(abi_events)),
            ("extra_uses".to_string(), extra_uses_node),
            (
                "generated_external_functions".to_string(),
                RewriteNode::new_modified(generated_external_functions),
            ),
            (
                "generated_l1_handler_functions".to_string(),
                RewriteNode::new_modified(generated_l1_handler_functions),
            ),
            (
                "generated_constructor_functions".to_string(),
                RewriteNode::new_modified(generated_constructor_functions),
            ),
        ]),
    );

    let mut builder = PatchBuilder::new(db);
    builder.add_modified(generated_contract_mod);
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "contract".into(),
            content: builder.code,
            aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(
                StarkNetContractAuxData {
                    patches: builder.patches,
                    contracts: vec![module_name_ast.text(db)],
                },
            )),
        }),
        diagnostics,
        remove_original_item: true,
    }
}

/// Validates the first parameter of an L1 handler is `from_address: felt252` or `_from_address:
/// felt252`.
fn validate_l1_handler_first_parameter(
    db: &dyn SyntaxGroup,
    params: &ast::ParamList,
    diagnostics: &mut Vec<PluginDiagnostic>,
) {
    if let Some(first_param) = params.elements(db).first() {
        // Validate type
        if !is_felt252(db, &first_param.type_clause(db).ty(db)) {
            diagnostics.push(PluginDiagnostic {
                message: "The first parameter of an L1 handler must be of type `felt252`."
                    .to_string(),
                stable_ptr: first_param.stable_ptr().untyped(),
            });
        }

        // Validate name
        if maybe_strip_underscore(first_param.name(db).text(db).as_str())
            != L1_HANDLER_FIRST_PARAM_NAME
        {
            diagnostics.push(PluginDiagnostic {
                message: "The first parameter of an L1 handler must be named 'from_address'."
                    .to_string(),
                stable_ptr: first_param.stable_ptr().untyped(),
            });
        }
    } else {
        diagnostics.push(PluginDiagnostic {
            message: "An L1 handler must have the 'from_address' parameter.".to_string(),
            stable_ptr: params.stable_ptr().untyped(),
        });
    };
}
