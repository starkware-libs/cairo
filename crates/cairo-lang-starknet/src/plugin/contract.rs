use std::collections::HashMap;
use std::vec;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::patcher::{ModifiedNode, PatchBuilder, RewriteNode};
use cairo_lang_semantic::plugin::DynPluginAuxData;
use cairo_lang_syntax::node::ast::{MaybeModuleBody, OptionWrappedGenericParamList};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use indoc::formatdoc;

use super::consts::{
    ABI_TRAIT, CONSTRUCTOR_ATTR, CONSTRUCTOR_MODULE, CONTRACT_ATTR, EVENT_ATTR, EXTERNAL_ATTR,
    EXTERNAL_MODULE, STORAGE_STRUCT_NAME, VIEW_ATTR,
};
use super::entry_point::generate_entry_point_wrapper;
use super::events::handle_event;
use super::storage::handle_storage_struct;
use crate::plugin::aux_data::StarkNetContractAuxData;

/// If the module is annotated with CONTRACT_ATTR, generate the relevant contract logic.
pub fn handle_mod(db: &dyn SyntaxGroup, module_ast: ast::ItemModule) -> PluginResult {
    if !module_ast.has_attr(db, CONTRACT_ATTR) {
        // TODO(ilya): diagnostic
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
    // Uses to do in generated inner modules so that everything that is available in the contract
    // module is still available.
    let mut extra_uses = vec![];
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
            // Externs, trait declrations and free functions are not directly required in generated
            // inner modules.
            ast::Item::ExternFunction(_)
            | ast::Item::ExternType(_)
            | ast::Item::Trait(_)
            | ast::Item::FreeFunction(_) => None,
        } {
            extra_uses.push(RewriteNode::Text(format!("\n        use super::{};", ident.text(db))));
        }
    }

    let extra_uses_node = RewriteNode::Modified(ModifiedNode { children: extra_uses });
    let mut generated_external_functions = Vec::new();
    let mut generated_constructor_functions = Vec::new();

    let mut storage_code = RewriteNode::Text("".to_string());
    let mut abi_functions = Vec::new();
    let mut event_functions = Vec::new();
    let mut abi_events = Vec::new();
    for item in body.items(db).elements(db) {
        match &item {
            ast::Item::FreeFunction(item_function)
                if item_function.has_attr(db, EXTERNAL_ATTR)
                    || item_function.has_attr(db, VIEW_ATTR)
                    || item_function.has_attr(db, CONSTRUCTOR_ATTR) =>
            {
                let attr = if item_function.has_attr(db, EXTERNAL_ATTR) {
                    EXTERNAL_ATTR
                } else if item_function.has_attr(db, VIEW_ATTR) {
                    VIEW_ATTR
                } else {
                    CONSTRUCTOR_ATTR
                };

                let declaration = item_function.declaration(db);
                if let OptionWrappedGenericParamList::WrappedGenericParamList(generic_params) =
                    declaration.generic_params(db)
                {
                    diagnostics.push(PluginDiagnostic {
                        message: "Contract entry points cannot have generic arguments".to_string(),
                        stable_ptr: generic_params.stable_ptr().untyped(),
                    })
                }
                abi_functions.push(RewriteNode::Modified(ModifiedNode {
                    children: vec![
                        RewriteNode::Text(format!("#[{attr}]\n        ")),
                        RewriteNode::Trimmed(declaration.as_syntax_node()),
                        RewriteNode::Text(";\n        ".to_string()),
                    ],
                }));

                match generate_entry_point_wrapper(db, item_function) {
                    Ok(generated_function) => {
                        let generated = if item_function.has_attr(db, CONSTRUCTOR_ATTR) {
                            &mut generated_constructor_functions
                        } else {
                            &mut generated_external_functions
                        };
                        generated.push(generated_function);
                        generated.push(RewriteNode::Text("\n        ".to_string()));
                    }
                    Err(entry_point_diagnostics) => {
                        diagnostics.extend(entry_point_diagnostics);
                    }
                }
            }
            ast::Item::FreeFunction(item_function) if item_function.has_attr(db, EVENT_ATTR) => {
                let (rewrite_nodes, event_diagnostics) = handle_event(db, item_function.clone());
                if let Some((event_function_rewrite, abi_event_rewrite)) = rewrite_nodes {
                    event_functions.push(event_function_rewrite);
                    // TODO(yuval): keep track in the ABI that these are events.
                    abi_events.push(abi_event_rewrite);
                }
                diagnostics.extend(event_diagnostics);
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
                    use starknet_serde::ContractAddressSerde;

                    $generated_external_functions$
                }}

                mod {CONSTRUCTOR_MODULE} {{$extra_uses$
                    use starknet_serde::ContractAddressSerde;

                    $generated_constructor_functions$
                }}
            }}
        "
        )
        .as_str(),
        HashMap::from([
            ("contract_name".to_string(), RewriteNode::Trimmed(module_name_ast.as_syntax_node())),
            (
                "original_items".to_string(),
                RewriteNode::Modified(ModifiedNode { children: kept_original_items }),
            ),
            ("storage_code".to_string(), storage_code),
            (
                "event_functions".to_string(),
                RewriteNode::Modified(ModifiedNode { children: event_functions }),
            ),
            (
                "abi_functions".to_string(),
                RewriteNode::Modified(ModifiedNode { children: abi_functions }),
            ),
            (
                "abi_events".to_string(),
                RewriteNode::Modified(ModifiedNode { children: abi_events }),
            ),
            ("extra_uses".to_string(), extra_uses_node),
            (
                "generated_external_functions".to_string(),
                RewriteNode::Modified(ModifiedNode { children: generated_external_functions }),
            ),
            (
                "generated_constructor_functions".to_string(),
                RewriteNode::Modified(ModifiedNode { children: generated_constructor_functions }),
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
