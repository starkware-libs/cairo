use std::vec;

use defs::db::{MacroPlugin, PluginDiagnostic, PluginResult};
use genco::prelude::*;
use itertools::join;
use syntax::node::ast::{ItemFreeFunction, MaybeModuleBody, Modifier, Param};
use syntax::node::db::SyntaxGroup;
use syntax::node::helpers::GetIdentifier;
use syntax::node::{ast, Terminal, TypedSyntaxNode};

use crate::contract::starknet_keccak;

pub static CONTRACT_ATTR: &str = "contract";
pub static EXTERNAL_ATTR: &str = "external";
pub static EXTERNAL_MODULE: &str = "__external";

#[cfg(test)]
#[path = "plugin_test.rs"]
mod test;

#[derive(Debug)]
pub struct StarkNetPlugin {}

impl MacroPlugin for StarkNetPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Module(module_ast) => handle_mod(db, module_ast),
            ast::Item::Struct(struct_ast) => handle_struct(db, struct_ast),
            // TODO(yuval): diagnostic
            _ => PluginResult::default(),
        }
    }
}

/// If the module is annotated with CONTRACT_ATTR, generate the relevant contract logic.
fn handle_mod(db: &dyn SyntaxGroup, module_ast: ast::ItemModule) -> PluginResult {
    let attrs = module_ast.attributes(db).elements(db);
    if !attrs.iter().any(|attr| attr.attr(db).text(db) == CONTRACT_ATTR) {
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
            };
        }
    };

    let contract_name = module_ast.name(db).text(db).to_string();
    let mut functions_tokens = rust::Tokens::new();
    for item in body.items(db).elements(db) {
        let item_function = match item {
            ast::Item::FreeFunction(f) => f,
            // Nothing to do for other items.
            _ => continue,
        };

        if item_function
            .attributes(db)
            .elements(db)
            .iter()
            .any(|attr| attr.attr(db).text(db) == EXTERNAL_ATTR)
        {
            // TODO(ilya): propagate the diagnostics in case of failure.
            if let Some(generated_function) =
                generate_entry_point_wrapper(db, &contract_name, &item_function)
            {
                functions_tokens.append(generated_function);
            }
        }
    }

    let external_entry_points: rust::Tokens = quote! {
        mod $EXTERNAL_MODULE {
            use super::$(contract_name);
            $functions_tokens
        }
    };

    PluginResult {
        code: Some(("entry_points".into(), external_entry_points.to_string().unwrap())),
        diagnostics: vec![],
    }
}

/// If the struct is annotated with CONTRACT_ATTR, generate getter functions for
/// its members.
fn handle_struct(db: &dyn SyntaxGroup, struct_ast: ast::ItemStruct) -> PluginResult {
    let attrs = struct_ast.attributes(db).elements(db);
    if !attrs.iter().any(|attr| attr.attr(db).text(db) == CONTRACT_ATTR) {
        // TODO(ilya): diagnostic
        return PluginResult::default();
    }

    let mut code_tokens = rust::Tokens::new();

    for member in struct_ast.members(db).elements(db) {
        let name = member.name(db).text(db).to_string();
        let address = format!("0x{:x}", starknet_keccak(name.as_bytes()));

        let generated_submodule = quote! {
            mod $name {
                func read(ref system: System) -> felt {
                    starknet::storage_read_syscall(
                        system, starknet::storage_address_const::<$(address.clone())>())
                }
                func write(ref system: System, value: felt) -> Result::<(), felt> {
                    starknet::storage_write_syscall(
                        system, starknet::storage_address_const::<$address>(), value)
                }
            }
        };

        code_tokens.append(generated_submodule)
    }
    PluginResult {
        code: Some(("contract_storage".into(), code_tokens.to_string().unwrap())),
        diagnostics: vec![],
    }
}

/// Generates Cairo code for an entry point wrapper.
fn generate_entry_point_wrapper(
    db: &dyn SyntaxGroup,
    contract_name: &str,
    function: &ItemFreeFunction,
) -> Option<rust::Tokens> {
    let mut successful_expansion = true;

    let sig = function.signature(db);
    let params = sig.parameters(db).elements(db);
    // TODO(yuval): support types of size >1. `params_len` should be sum of the params lengths.
    let params_len = params.len();
    let mut params_iter = params.into_iter();

    // Assert the first parameter is System.
    // TODO(yuval): get_text includes trivia... This wouldn't always work.
    match params_iter.next() {
        Some(first_param) => {
            let first_param_type = first_param.type_clause(db).ty(db).as_syntax_node().get_text(db);
            if first_param_type != "System" {
                // TODO(yuval): diagnostic
                successful_expansion = false;
            }
            if !is_ref_param(db, &first_param) {
                // TODO(yuval): diagnostic
                successful_expansion = false;
            }
        }
        None => {
            // TODO(yuval): diagnostic
            successful_expansion = false;
        }
    }

    if !successful_expansion {
        return None;
    }

    let mut arg_names = Vec::new();
    let mut arg_definitions = quote! {};
    let mut ref_appends = quote! {};
    for (idx, param) in params_iter.enumerate() {
        let arg_name = format!("__arg_{}", param.name(db).identifier(db));
        let is_ref = is_ref_param(db, &param);

        arg_names.push(arg_name.clone());
        let mut_modifier = if is_ref { "mut " } else { "" };
        // TODO(yuval): use panicable version of `array_at` once panic_with supports generic
        // params.
        arg_definitions.append(quote! {let $mut_modifier$(arg_name.clone()): felt = match array::array_at::<felt>(data, $(idx)_u128) {
                    Option::Some(x) => x,
                    Option::None(()) => panic(array::array_new::<felt>()),
                };});

        if is_ref {
            // TODO(yuval): support types != felt.
            ref_appends.append(quote! {array::array_append::<felt>(arr, $arg_name);});
        }
    }
    let param_names_tokens = join(arg_names.into_iter(), ", ");

    let function_name = function.name(db).text(db).to_string();
    let wrapped_name = format!("{contract_name}::{function_name}");

    Some(quote! {
        func $function_name(ref system: System, mut data: Array::<felt>) -> Array::<felt> {
            if array::array_len::<felt>(data) != $(params_len)_u128 {
                // TODO(yuval): add error message.
                panic(array::array_new::<felt>());
            }
            $arg_definitions
            let res = $wrapped_name(system, $param_names_tokens);
            let mut arr = array::array_new::<felt>();
            $ref_appends
            array::array_append::<felt>(arr, res);
            arr
        }
    })
}

/// Checks if the parameter is defined as a ref parameter.
fn is_ref_param(db: &dyn SyntaxGroup, param: &Param) -> bool {
    let param_modifiers = param.modifiers(db).elements(db);
    // TODO(yuval): This works only if "ref" is the only modifier. If the expansion was at the
    // semantic level, we could just ask if it's a reference.
    param_modifiers.len() == 1 && matches!(param_modifiers[0], Modifier::Ref(_))
}
