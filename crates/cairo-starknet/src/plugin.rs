use std::sync::Arc;
use std::vec;

use cairo_defs::plugin::{
    DynGeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin, TrivialMapper};
use cairo_syntax::node::ast::{
    ItemFreeFunction, MaybeModuleBody, Modifier, OptionReturnTypeClause, Param,
};
use cairo_syntax::node::db::SyntaxGroup;
use cairo_syntax::node::helpers::GetIdentifier;
use cairo_syntax::node::{ast, Terminal, TypedSyntaxNode};
use genco::prelude::*;
use itertools::join;

use crate::contract::starknet_keccak;

pub const CONTRACT_ATTR: &str = "contract";
const EXTERNAL_ATTR: &str = "external";
const VIEW_ATTR: &str = "view";
pub const ABI_TRAIT: &str = "__abi";
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
impl AsDynMacroPlugin for StarkNetPlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}
impl SemanticPlugin for StarkNetPlugin {}

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
    let mut external_declarations = rust::Tokens::new();
    for item in body.items(db).elements(db) {
        match &item {
            ast::Item::FreeFunction(item_function)
                if item_function.attributes(db).elements(db).iter().any(|attr| {
                    matches!(attr.attr(db).text(db).as_str(), EXTERNAL_ATTR | VIEW_ATTR)
                }) =>
            {
                {
                    // TODO(yuval): change to item_function.declaration
                    let declaration = item_function.declaration(db).as_syntax_node().get_text(db);
                    external_declarations.append(quote! {$declaration;});

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

    let generated_contract_mod: rust::Tokens = quote! {
        mod __generated__$contract_name {
            $original_items

            // TODO(yuval): consider adding and impl of __abi and use it from the wrappers, instead
            // of the original functions (they can be removed).
            trait $ABI_TRAIT {
                $external_declarations
            }

            mod $EXTERNAL_MODULE {
                $generated_external_functions
            }
        }
    };

    let contract_code =
        format!("{}\n{}", storage_code, generated_contract_mod.to_string().unwrap());

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "contract".into(),
            content: contract_code,
            aux_data: DynGeneratedFileAuxData(Arc::new(TrivialMapper {})),
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
    let declaration = function.declaration(db);
    let sig = declaration.signature(db);
    let params = sig.parameters(db).elements(db);

    let mut arg_names = Vec::new();
    let mut arg_definitions = quote! {};
    let mut ref_appends = quote! {};
    let input_data_short_err = "'Input too short for arguments'";
    for param in params {
        let arg_name = format!("__arg_{}", param.name(db).identifier(db));
        let type_name = param.type_clause(db).ty(db).as_syntax_node().get_text(db);
        // TODO(orizi): Use traits for serialization when supported.
        let ser_func = format!("serde::serialize_{type_name}");
        let deser_func = format!("serde::deserialize_{type_name}");
        let is_ref = is_ref_param(db, &param);

        arg_names.push(arg_name.clone());
        let mut_modifier = if is_ref { "mut " } else { "" };
        // TODO(yuval): use panicable version of deserializations when supported.
        arg_definitions.append(
            quote! {let $mut_modifier$(arg_name.clone()) = match $deser_func(data) {
                Option::Some(x) => x,
                Option::None(()) => {
                    let mut err_data = array_new::<felt>();
                    array_append::<felt>(err_data, $input_data_short_err);
                    panic(err_data)
                },
            };},
        );

        if is_ref {
            ref_appends.append(quote! {$ser_func(arr, $arg_name);});
        }
    }
    let param_names_tokens = join(arg_names.into_iter(), ", ");

    let function_name = declaration.name(db).text(db).to_string();
    let wrapped_name = format!("super::{function_name}");
    let (let_res, append_res) = match sig.ret_ty(db) {
        OptionReturnTypeClause::Empty(_) => ("", "".to_string()),
        OptionReturnTypeClause::ReturnTypeClause(ty) => {
            let ret_type_name = ty.ty(db).as_syntax_node().get_text(db);
            ("let res = ", format!("serde::serialize_{ret_type_name}(arr, res)"))
        }
    };

    let oog_err = "'Out of gas'";
    let input_data_long_err = "'Input too long for arguments'";
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
            $arg_definitions
            if array_len::<felt>(data) != 0_u128 {
                let mut err_data = array_new::<felt>();
                array_append::<felt>(err_data, $input_data_long_err);
                panic(err_data);
            }
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
