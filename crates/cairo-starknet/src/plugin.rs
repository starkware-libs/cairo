use std::collections::HashMap;
use std::sync::Arc;
use std::vec;

use cairo_defs::plugin::{
    DynGeneratedFileAuxData, GeneratedFileAuxData, MacroPlugin, PluginDiagnostic,
    PluginGeneratedFile, PluginResult,
};
use cairo_diagnostics::DiagnosticEntry;
use cairo_semantic::db::SemanticGroup;
use cairo_semantic::patcher::{ModifiedNode, PatchBuilder, RewriteNode};
use cairo_semantic::plugin::{
    AsDynGeneratedFileAuxData, AsDynMacroPlugin, DiagnosticMapper, DynDiagnosticMapper,
    PluginMappedDiagnostic, SemanticPlugin, TrivialMapper,
};
use cairo_semantic::SemanticDiagnostic;
use cairo_syntax::node::ast::{
    ItemFreeFunction, MaybeModuleBody, MaybeTraitBody, Modifier, OptionReturnTypeClause, Param,
};
use cairo_syntax::node::db::SyntaxGroup;
use cairo_syntax::node::helpers::{GetIdentifier, QueryAttrs};
use cairo_syntax::node::{ast, Terminal, TypedSyntaxNode};
use genco::prelude::*;
use indoc::formatdoc;
use itertools::join;

use crate::contract::starknet_keccak;

const ABI_ATTR: &str = "abi";
const CONTRACT_ATTR: &str = "contract";
const EXTERNAL_ATTR: &str = "external";
const VIEW_ATTR: &str = "view";
pub const GENERATED_CONTRACT_ATTR: &str = "generated_contract";
pub const ABI_TRAIT: &str = "__abi";
pub const EXTERNAL_MODULE: &str = "__external";

use cairo_semantic::patcher::Patches;

/// The diagnostics remapper of the pluging.
#[derive(Debug, PartialEq, Eq)]
pub struct DiagnosticRemapper {
    patches: Patches,
}
impl GeneratedFileAuxData for DiagnosticRemapper {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
    }
}
impl AsDynGeneratedFileAuxData for DiagnosticRemapper {
    fn as_dyn_macro_token(&self) -> &(dyn GeneratedFileAuxData + 'static) {
        self
    }
}
impl DiagnosticMapper for DiagnosticRemapper {
    fn map_diag(
        &self,
        db: &(dyn SemanticGroup + 'static),
        diag: &dyn std::any::Any,
    ) -> Option<PluginMappedDiagnostic> {
        let Some(diag) = diag.downcast_ref::<SemanticDiagnostic>() else {return None;};
        let span = self
            .patches
            .translate(db.upcast(), diag.stable_location.diagnostic_location(db.upcast()).span)?;
        Some(PluginMappedDiagnostic { span, message: diag.format(db) })
    }
}

#[cfg(test)]
#[path = "plugin_test.rs"]
mod test;

#[derive(Debug)]
pub struct StarkNetPlugin {}

impl MacroPlugin for StarkNetPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Module(module_ast) => handle_mod(db, module_ast),
            ast::Item::Trait(trait_ast) => handle_trait(db, trait_ast),
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

/// If the trait is annotated with ABI_ATTR, generate the relevant dispatcher logic.
fn handle_trait(db: &dyn SyntaxGroup, trait_ast: ast::ItemTrait) -> PluginResult {
    let attrs = trait_ast.attributes(db).elements(db);
    if !attrs.iter().any(|attr| attr.attr(db).text(db) == ABI_ATTR) {
        return PluginResult::default();
    }
    let body = match trait_ast.body(db) {
        MaybeTraitBody::Some(body) => body,
        MaybeTraitBody::None(empty_body) => {
            return PluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic {
                    message: "ABIs without body are not supported.".to_string(),
                    stable_ptr: empty_body.stable_ptr().untyped(),
                }],
                remove_original_item: false,
            };
        }
    };

    let mut diagnostics = vec![];
    let mut functions = vec![];
    for item_ast in body.items(db).elements(db) {
        match item_ast {
            ast::TraitItem::Function(func) => {
                let declaration = func.declaration(db);

                let mut has_ref_params = false;
                for param in declaration.signature(db).parameters(db).elements(db) {
                    if is_ref_param(db, &param) {
                        has_ref_params = true;

                        diagnostics.push(PluginDiagnostic {
                            message: "`ref` parameters are not supported in the ABI of a contract."
                                .to_string(),
                            stable_ptr: param.modifiers(db).stable_ptr().untyped(),
                        })
                    }
                }
                if has_ref_params {
                    // TODO(ilya): Consider generating an empty wrapper to avoid:
                    // Unknown function error.
                    continue;
                }

                let mut func_declaration = RewriteNode::from_ast(&declaration);

                func_declaration
                    .modify_child(db, ast::FunctionDeclaration::INDEX_SIGNATURE)
                    .modify_child(db, ast::FunctionSignature::INDEX_PARAMETERS)
                    .modify(db)
                    .children
                    .splice(
                        0..0,
                        [
                            RewriteNode::Text("contract_address: ContractAddress".to_string()),
                            RewriteNode::Text(", ".to_string()),
                        ],
                    );

                let func_body = " {
        let calldata = array_new::<felt>();
        // TODO(ilya): Encode calldata.
        let ret_data = match starknet::call_contract_syscall(
                contract_address, calldata) {
            Result::Ok(ret_data) => ret_data,
            Result::Err((reason, _ret_data)) => {
                let mut err_data = array_new::<felt>();
                array_append::<felt>(err_data, 'call_contract_syscall failed');
                array_append::<felt>(err_data, reason);
                // TODO(ilya): Handle ret_data.
                panic(err_data)
            }
        };
        // TODO(ilya): Decode ret_data and return it.
    }
";

                functions.push(RewriteNode::Modified(ModifiedNode {
                    children: vec![
                        RewriteNode::from_ast(&func.attributes(db)),
                        func_declaration,
                        RewriteNode::Text(func_body.to_string()),
                    ],
                }))
            }
        }
    }

    let dispatcher_name = format!("{}Dispatcher", trait_ast.name(db).text(db));
    let dispatcher_code = RewriteNode::interpolate_patched(
        &formatdoc!(
            "mod {dispatcher_name} {{
                $body$
            }}",
        ),
        HashMap::from([(
            "body".to_string(),
            RewriteNode::Modified(ModifiedNode { children: functions }),
        )]),
    );
    let mut builder = PatchBuilder::new(db);
    builder.add_modified(dispatcher_code);
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: dispatcher_name.into(),
            content: builder.code,
            aux_data: DynGeneratedFileAuxData::new(DynDiagnosticMapper::new(DiagnosticRemapper {
                patches: builder.patches,
            })),
        }),
        diagnostics,
        remove_original_item: false,
    }
}

/// If the module is annotated with CONTRACT_ATTR, generate the relevant contract logic.
fn handle_mod(db: &dyn SyntaxGroup, module_ast: ast::ItemModule) -> PluginResult {
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

    let contract_name = module_ast.name(db).text(db).to_string();
    let mut generated_external_functions = rust::Tokens::new();

    let mut storage_code = "".to_string();
    let mut original_items = rust::Tokens::new();
    let mut external_declarations = rust::Tokens::new();
    for item in body.items(db).elements(db) {
        match &item {
            ast::Item::FreeFunction(item_function)
                if item_function.has_attr(db, EXTERNAL_ATTR)
                    || item_function.has_attr(db, VIEW_ATTR) =>
            {
                let declaration = item_function.declaration(db).as_syntax_node().get_text(db);
                external_declarations.append(quote! {$declaration;});
                match generate_entry_point_wrapper(db, item_function) {
                    Ok(generated_function) => {
                        generated_external_functions.append(generated_function);
                    }
                    Err(entry_point_diagnostics) => {
                        diagnostics.extend(entry_point_diagnostics);
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
        #[$GENERATED_CONTRACT_ATTR]
        mod $contract_name {
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
            // TODO(ilya): Remove formatting once the plugin output is readable.
            content: cairo_formatter::format_string(db, contract_code),
            aux_data: DynGeneratedFileAuxData(Arc::new(TrivialMapper {})),
        }),
        diagnostics,
        remove_original_item: true,
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

/// Returns the serde functions for a type.
// TODO(orizi): Use type ids when semantic information is available.
// TODO(orizi): Use traits for serialization when supported.
fn get_type_serde_funcs(name: &str) -> Option<(&str, &str)> {
    match name.trim() {
        "felt" => Some(("serde::serialize_felt", "serde::deserialize_felt")),
        "bool" => Some(("serde::serialize_bool", "serde::deserialize_bool")),
        "u128" => Some(("serde::serialize_u128", "serde::deserialize_u128")),
        "u256" => Some(("serde::serialize_u256", "serde::deserialize_u256")),
        "Array::<felt>" => Some(("serde::serialize_array_felt", "serde::deserialize_array_felt")),
        _ => None,
    }
}

/// Generates Cairo code for an entry point wrapper.
fn generate_entry_point_wrapper(
    db: &dyn SyntaxGroup,
    function: &ItemFreeFunction,
) -> Result<rust::Tokens, Vec<PluginDiagnostic>> {
    let declaration = function.declaration(db);
    let sig = declaration.signature(db);
    let params = sig.parameters(db).elements(db);
    let mut diagnostics = vec![];
    let mut arg_names = Vec::new();
    let mut arg_definitions = quote! {};
    let mut ref_appends = quote! {};
    let input_data_short_err = "'Input too short for arguments'";
    for param in params {
        let arg_name = format!("__arg_{}", param.name(db).identifier(db));
        let arg_type_ast = param.type_clause(db).ty(db);
        let type_name = arg_type_ast.as_syntax_node().get_text(db);
        let Some((ser_func, deser_func)) = get_type_serde_funcs(&type_name) else {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: arg_type_ast.stable_ptr().0,
                message: format!("Could not find serialization for type `{type_name}`"),
            });
            continue;
        };

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
            let ret_type_ast = ty.ty(db);
            let ret_type_name = ret_type_ast.as_syntax_node().get_text(db);
            // TODO(orizi): Handle tuple types.
            if let Some((ser_func, _)) = get_type_serde_funcs(&ret_type_name) {
                ("let res = ", format!("{ser_func}(arr, res)"))
            } else {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: ret_type_ast.stable_ptr().0,
                    message: format!("Could not find serialization for type `{ret_type_name}`"),
                });
                ("", "".to_string())
            }
        }
    };
    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    let oog_err = "'Out of gas'";
    let input_data_long_err = "'Input too long for arguments'";
    Ok(quote! {
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
                // Force the inclusion of `System` in the list of implicits.
                starknet::use_system_implicit();

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
