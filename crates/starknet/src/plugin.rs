use std::vec;

use defs::plugin::{
    DynDiagnosticMapper, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
    TrivialMapper,
};
use genco::prelude::*;
use itertools::join;
use syntax::node::ast::{
    ItemFreeFunction, MaybeModuleBody, Modifier, OptionReturnTypeClause, Param,
};
use syntax::node::db::SyntaxGroup;
use syntax::node::helpers::GetIdentifier;
use syntax::node::{ast, Terminal, TypedSyntaxNode};

use crate::contract::starknet_keccak;

pub const CONTRACT_ATTR: &str = "contract";
const EXTERNAL_ATTR: &str = "external";
const VIEW_ATTR: &str = "view";
pub const EXTERNAL_MODULE: &str = "__external";

#[cfg(test)]
#[path = "plugin_test.rs"]
mod test;

#[derive(Debug)]
pub struct StarkNetPlugin {}

impl MacroPlugin for StarkNetPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Module(module_ast) => handle_mod(db, module_ast),
            // Nothing to do for other items.
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
    let mut generated_external_functions = rust::Tokens::new();

    let mut storage_code = "".to_string();
    let mut original_items = rust::Tokens::new();
    for item in body.items(db).elements(db) {
        match &item {
            ast::Item::FreeFunction(item_function)
                if item_function.attributes(db).elements(db).iter().any(|attr| {
                    matches!(attr.attr(db).text(db).as_str(), EXTERNAL_ATTR | VIEW_ATTR)
                }) =>
            {
                {
                    // TODO(ilya): propagate the diagnostics in case of failure.
                    if let Some(generated_function) =
                        generate_entry_point_wrapper(db, item_function)
                    {
                        generated_external_functions.append(generated_function);
                    }
                }
            }
            ast::Item::Struct(item_struct) if item_struct.name(db).text(db) == "Storage" => {
                storage_code = handle_storage_struct(db, item_struct.clone());
            }
            _ => {}
        };
        let orig_text = item.as_syntax_node().get_text(db);
        original_items.append(quote! {$orig_text})
    }

    let external_entry_points: rust::Tokens = quote! {
        mod __generated__$contract_name {
            $original_items
            mod $EXTERNAL_MODULE {
                $generated_external_functions
            }
        }
    };

    let contract_code = format!("{}\n{}", storage_code, external_entry_points.to_string().unwrap());

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "contract".into(),
            content: contract_code,
            diagnostic_mapper: DynDiagnosticMapper::new(TrivialMapper {}),
        }),
        diagnostics: vec![],
    }
}

/// Generate getters and setters for the variables in the storage struct.
fn handle_storage_struct(db: &dyn SyntaxGroup, struct_ast: ast::ItemStruct) -> String {
    let mut code_tokens = rust::Tokens::new();

    for member in struct_ast.members(db).elements(db) {
        let name = member.name(db).text(db).to_string();
        let address = format!("0x{:x}", starknet_keccak(name.as_bytes()));

        let generated_submodule = quote! {
            mod $name {
                fn read() -> felt {
                    starknet::storage_read_syscall(
                        starknet::storage_address_const::<$(address.clone())>())
                }
                fn write(value: felt) -> Result::<(), felt> {
                    starknet::storage_write_syscall(
                        starknet::storage_address_const::<$address>(), value)
                }
            }
        };

        code_tokens.append(generated_submodule)
    }
    code_tokens.to_string().unwrap()
}

/// Generates Cairo code for an entry point wrapper.
fn generate_entry_point_wrapper(
    db: &dyn SyntaxGroup,
    function: &ItemFreeFunction,
) -> Option<rust::Tokens> {
    let sig = function.signature(db);
    let params = sig.parameters(db).elements(db);
    // TODO(yuval): support types of size >1. `params_len` should be sum of the params lengths.
    let params_len = params.len();

    let mut arg_names = Vec::new();
    let mut arg_definitions = quote! {};
    let mut ref_appends = quote! {};
    for (idx, param) in params.into_iter().enumerate() {
        let arg_name = format!("__arg_{}", param.name(db).identifier(db));
        let is_ref = is_ref_param(db, &param);

        arg_names.push(arg_name.clone());
        let mut_modifier = if is_ref { "mut " } else { "" };
        // TODO(yuval): use panicable version of `array_at` once panic_with supports generic
        // params.
        arg_definitions.append(quote! {let $mut_modifier$(arg_name.clone()): felt = match array_at::<felt>(data, $(idx)_u128) {
                    Option::Some(x) => x,
                    Option::None(()) => panic(array_new::<felt>()),
                };});

        if is_ref {
            // TODO(yuval): support types != felt.
            ref_appends.append(quote! {array_append::<felt>(arr, $arg_name);});
        }
    }
    let param_names_tokens = join(arg_names.into_iter(), ", ");

    let function_name = function.name(db).text(db).to_string();
    let wrapped_name = format!("super::{function_name}");

    let (let_res, append_res) = match sig.ret_ty(db) {
        OptionReturnTypeClause::Empty(_) => ("", ""),
        OptionReturnTypeClause::ReturnTypeClause(_) => {
            ("let res = ", "array_append::<felt>(arr, res);")
        }
    };

    let oog_err = "'Out of gas'";
    let wrong_num_args_err = "'Wrong number of arguments'";
    Some(quote! {
        fn $function_name(mut data: Array::<felt>) -> Array::<felt> {
            // TODO(yuval): use panicable version of `get_gas` once inlining is supported.
            match get_gas() {
                Option::Some(_) => {},
                Option::None(_) => {
                    let mut err_data = array_new::<felt>();
                    array_append::<felt>(err_data, $oog_err);
                    panic(err_data);
                },
            }

            if array_len::<felt>(data) != $(params_len)_u128 {
                let mut err_data = array_new::<felt>();
                array_append::<felt>(err_data, $wrong_num_args_err);
                panic(err_data);
            }
            $arg_definitions
            $let_res $wrapped_name($param_names_tokens);
            let mut arr = array_new::<felt>();
            $ref_appends
            $append_res
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
