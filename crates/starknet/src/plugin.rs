use defs::db::{MacroPlugin, PluginResult};
use genco::prelude::*;
use itertools::join;
use syntax::node::ast::{ItemFreeFunction, MaybeImplBody, Modifier, Param};
use syntax::node::db::SyntaxGroup;
use syntax::node::helpers::GetIdentifier;
use syntax::node::{ast, Terminal, TypedSyntaxNode};

use crate::contract::starknet_keccak;

static CONTRACT_IMPL_ATTR: &str = "ContractImpl";
pub static WRAPPER_PREFIX: &str = "__wrapper_";
static CONTRACT_ATTR: &str = "contract";

#[cfg(test)]
#[path = "plugin_test.rs"]
mod test;

#[derive(Debug)]
pub struct StarkNetPlugin {}

impl MacroPlugin for StarkNetPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Impl(impl_ast) => handle_impl(db, impl_ast),
            ast::Item::Struct(struct_ast) => handle_struct(db, struct_ast),
            // TODO(yuval): diagnostic
            _ => PluginResult::default(),
        }
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
        // TODO(ilya): Put this logic inside an inline module once inline modules are supported.
        let name = member.name(db).text(db);
        let address = format!("0x{:x}", starknet_keccak(name.as_bytes()));
        let getter_name = format!("get_{}", name);

        let getter = quote! {
            func $getter_name(ref syscall_ptr: SyscallPtr) -> felt {
                starknet::storage_read_syscall(
                    syscall_ptr, starknet::storage_address_const::<$address>())
            }
        };

        code_tokens.append(getter)
    }
    PluginResult {
        code: Some(("contract_storage".into(), code_tokens.to_string().unwrap())),
        diagnostics: vec![],
    }
}

/// If the impl is annotated with CONTRACT_IMPL_ATTR, generates wrappers for
/// the functions in the impl.
fn handle_impl(db: &dyn SyntaxGroup, impl_ast: ast::ItemImpl) -> PluginResult {
    let attrs = impl_ast.attributes(db).elements(db);
    if !attrs.iter().any(|attr| attr.attr(db).text(db) == CONTRACT_IMPL_ATTR) {
        // TODO(yuval): diagnostic
        return PluginResult::default();
    }
    let body = match impl_ast.body(db) {
        MaybeImplBody::Some(body) => body,
        // TODO(yuval): diagnostic
        MaybeImplBody::None(_) => return PluginResult::default(),
    };
    let impl_items = body.items(db);
    let mut functions_tokens = rust::Tokens::new();
    functions_tokens.append(impl_items.as_syntax_node().get_text(db).as_str());
    for impl_item in impl_items.elements(db) {
        let item_function = match impl_item {
            ast::Item::FreeFunction(f) => f,
            // Impl should only have free functions.
            _ => unreachable!(),
        };
        if let Some(generated_function) = generate_entry_point_wrapper(db, &item_function) {
            functions_tokens.append(generated_function);
        }
    }
    PluginResult {
        code: Some(("entry_points".into(), functions_tokens.to_string().unwrap())),
        diagnostics: vec![],
    }
}

/// Generates Cairo code for an entry point wrapper (expanded from attribute CONTRACT_IMPL_ATTR).
fn generate_entry_point_wrapper(
    db: &dyn SyntaxGroup,
    function: &ItemFreeFunction,
) -> Option<rust::Tokens> {
    let mut successful_expansion = true;

    let sig = function.signature(db);
    let params = sig.parameters(db).elements(db);
    // TODO(yuval): support types of size >1. `params_len` should be sum of the params lengths.
    let params_len = params.len();
    let mut params_iter = params.into_iter();

    // Assert the first parameter is SyscallPtr.
    // TODO(yuval): get_text includes trivia... This wouldn't always work.
    match params_iter.next() {
        Some(first_param) => {
            let first_param_type = first_param.type_clause(db).ty(db).as_syntax_node().get_text(db);
            if first_param_type != "SyscallPtr" {
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
        arg_definitions.append(quote! {let $mut_modifier$(arg_name.clone()): felt = match array::array_at::<felt>(data, $idx) {
                    Some(x) => x,
                    None(()) => panic(array::array_new::<felt>()),
                };});

        if is_ref {
            // TODO(yuval): support types != felt.
            ref_appends.append(quote! {array::array_append::<felt>(arr, $arg_name);});
        }
    }
    let param_names_tokens = join(arg_names.into_iter(), ", ");

    let wrapped_name = function.name(db).text(db).to_string();
    let wrapper_name = format!("{}{}", WRAPPER_PREFIX, wrapped_name);

    // TODO(yuval): change to uint128 literal once it's supported.
    Some(quote! {
        func $wrapper_name(ref syscall_ptr: SyscallPtr, mut data: Array::<felt>) -> Array::<felt> {
            if array::array_len::<felt>(data) != integer::uint128_from_felt($params_len) {
                // TODO(yuval): add error message.
                panic(array::array_new::<felt>());
            }
            $arg_definitions
            let res = $wrapped_name(syscall_ptr, $param_names_tokens);
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
