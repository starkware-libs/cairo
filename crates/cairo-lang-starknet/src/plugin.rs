use std::collections::HashMap;
use std::sync::Arc;
use std::vec;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, GeneratedFileAuxData, MacroPlugin, PluginDiagnostic,
    PluginGeneratedFile, PluginResult,
};
use cairo_lang_diagnostics::DiagnosticEntry;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::patcher::{ModifiedNode, PatchBuilder, Patches, RewriteNode};
use cairo_lang_semantic::plugin::{
    AsDynGeneratedFileAuxData, AsDynMacroPlugin, DiagnosticMapper, DynDiagnosticMapper,
    PluginMappedDiagnostic, SemanticPlugin,
};
use cairo_lang_semantic::SemanticDiagnostic;
use cairo_lang_syntax::node::ast::{
    FunctionWithBody, MaybeModuleBody, MaybeTraitBody, Modifier, OptionReturnTypeClause, Param,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;
use indoc::formatdoc;

use crate::contract::starknet_keccak;

const ABI_ATTR: &str = "abi";
const CONTRACT_ATTR: &str = "contract";
const EXTERNAL_ATTR: &str = "external";
pub const VIEW_ATTR: &str = "view";
pub const EVENT_ATTR: &str = "event";
pub const GENERATED_CONTRACT_ATTR: &str = "generated_contract";
pub const ABI_TRAIT: &str = "__abi";
pub const EXTERNAL_MODULE: &str = "__external";

/// The diagnostics remapper of the plugin.
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
                // Ignore events.
                if func.has_attr(db, EVENT_ATTR) {
                    continue;
                }

                let declaration = func.declaration(db);

                let mut skip_generation = false;
                let mut serialization_code = vec![];
                let signature = declaration.signature(db);
                for param in signature.parameters(db).elements(db) {
                    if is_ref_param(db, &param) {
                        skip_generation = true;

                        diagnostics.push(PluginDiagnostic {
                            message: "`ref` parameters are not supported in the ABI of a contract."
                                .to_string(),
                            stable_ptr: param.modifiers(db).stable_ptr().untyped(),
                        })
                    }

                    let param_type = param.type_clause(db).ty(db);
                    let type_name = &param_type.as_syntax_node().get_text(db);
                    serialization_code.push(RewriteNode::interpolate_patched(
                        &formatdoc!(
                            "        serde::Serde::<{type_name}>::serialize(ref calldata, \
                             $arg_name$);\n"
                        ),
                        HashMap::from([(
                            "arg_name".to_string(),
                            RewriteNode::Trimmed(param.name(db).as_syntax_node()),
                        )]),
                    ));
                }

                if skip_generation {
                    // TODO(ilya): Consider generating an empty wrapper to avoid:
                    // Unknown function error.
                    continue;
                }

                let ret_decode = match signature.ret_ty(db) {
                    OptionReturnTypeClause::Empty(_) => "".to_string(),
                    OptionReturnTypeClause::ReturnTypeClause(ty) => {
                        let ret_type_ast = ty.ty(db);
                        let type_name = ret_type_ast.as_syntax_node().get_text(db);
                        format!("        serde::Serde::<{type_name}>::deserialize(ref ret_data)")
                    }
                };

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

                functions.push(RewriteNode::interpolate_patched(
                    "$func_decl$ {
        let calldata = array_new();
$serialization_code$
        let ret_data = match starknet::call_contract_syscall(
            contract_address,
            calldata,
        ) {
            Result::Ok(ret_data) => ret_data,
            Result::Err((reason, _ret_data)) => {
                let mut err_data = array_new();
                array_append(ref err_data, 'call_contract_syscall failed');
                array_append(ref err_data, reason);
                // TODO(ilya): Handle ret_data.
                panic(err_data)
            },
        };
$deserialization_code$
    }
",
                    HashMap::from([
                        ("func_decl".to_string(), func_declaration),
                        (
                            "serialization_code".to_string(),
                            RewriteNode::Modified(ModifiedNode { children: serialization_code }),
                        ),
                        ("deserialization_code".to_string(), RewriteNode::Text(ret_decode)),
                    ]),
                ));
            }
        }
    }

    let mut builder = PatchBuilder::new(db);
    let dispatcher_name = format!("{}Dispatcher", trait_ast.name(db).text(db));
    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "mod {dispatcher_name} {{
                $body$
            }}",
        ),
        HashMap::from([(
            "body".to_string(),
            RewriteNode::Modified(ModifiedNode { children: functions }),
        )]),
    ));
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

    let mut generated_external_functions = Vec::new();

    let mut storage_code = RewriteNode::Text("".to_string());
    let mut original_items = Vec::new();
    let mut abi_functions = Vec::new();
    let mut event_functions = Vec::new();
    let mut abi_events = Vec::new();
    for item in body.items(db).elements(db) {
        let keep_original = match &item {
            ast::Item::FreeFunction(item_function)
                if item_function.has_attr(db, EXTERNAL_ATTR)
                    || item_function.has_attr(db, VIEW_ATTR) =>
            {
                let attr =
                    if item_function.has_attr(db, EXTERNAL_ATTR) { "external" } else { "view" };
                abi_functions.push(RewriteNode::Modified(ModifiedNode {
                    children: vec![
                        RewriteNode::Text(format!("#[{attr}]\n        ")),
                        RewriteNode::Trimmed(item_function.declaration(db).as_syntax_node()),
                        RewriteNode::Text(";\n        ".to_string()),
                    ],
                }));

                match generate_entry_point_wrapper(db, item_function) {
                    Ok(generated_function) => {
                        generated_external_functions.push(generated_function);
                        generated_external_functions
                            .push(RewriteNode::Text("\n        ".to_string()));
                    }
                    Err(entry_point_diagnostics) => {
                        diagnostics.extend(entry_point_diagnostics);
                    }
                }

                true
            }
            ast::Item::FreeFunction(item_function) if item_function.has_attr(db, EVENT_ATTR) => {
                let (rewrite_nodes, event_diagnostics) = handle_event(db, item_function.clone());
                if let Some((event_function_rewrite, abi_event_rewrite)) = rewrite_nodes {
                    event_functions.push(event_function_rewrite);
                    // TODO(yuval): keep track in the ABI that these are events.
                    abi_events.push(abi_event_rewrite);
                }
                diagnostics.extend(event_diagnostics);
                false
            }
            ast::Item::Struct(item_struct) if item_struct.name(db).text(db) == "Storage" => {
                let (storage_rewrite_node, storage_diagnostics) =
                    handle_storage_struct(db, item_struct.clone());
                storage_code = storage_rewrite_node;
                diagnostics.extend(storage_diagnostics);
                false
            }
            _ => true,
        };
        if keep_original {
            original_items.push(RewriteNode::Copied(item.as_syntax_node()));
        }
    }

    let generated_contract_mod = RewriteNode::interpolate_patched(
        formatdoc!(
            "
            #[{GENERATED_CONTRACT_ATTR}]
            mod $contract_name$ {{
            $original_items$
                $storage_code$
                trait {ABI_TRAIT} {{
                    $abi_functions$
                    $abi_events$
                }}

                mod {EXTERNAL_MODULE} {{
                    $generated_external_functions$
                    $event_functions$
                }}
            }}
        "
        )
        .as_str(),
        HashMap::from([
            (
                "contract_name".to_string(),
                RewriteNode::Trimmed(module_ast.name(db).as_syntax_node()),
            ),
            (
                "original_items".to_string(),
                RewriteNode::Modified(ModifiedNode { children: original_items }),
            ),
            ("storage_code".to_string(), storage_code),
            (
                "abi_functions".to_string(),
                RewriteNode::Modified(ModifiedNode { children: abi_functions }),
            ),
            (
                "abi_events".to_string(),
                RewriteNode::Modified(ModifiedNode { children: abi_events }),
            ),
            (
                "generated_external_functions".to_string(),
                RewriteNode::Modified(ModifiedNode { children: generated_external_functions }),
            ),
            (
                "event_functions".to_string(),
                RewriteNode::Modified(ModifiedNode { children: event_functions }),
            ),
        ]),
    );

    let mut builder = PatchBuilder::new(db);
    builder.add_modified(generated_contract_mod);

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "contract".into(),
            content: builder.code,
            aux_data: DynGeneratedFileAuxData::new(DynDiagnosticMapper::new(DiagnosticRemapper {
                patches: builder.patches,
            })),
        }),
        diagnostics,
        remove_original_item: true,
    }
}

/// Generates a function to emit an event and the corresponding ABI item.
/// On success, returns a RewriteNode for the event function and a RewriteNode for the ABI
/// declaration. On failure returns None. In addition, returns diagnostics.
fn handle_event(
    db: &dyn SyntaxGroup,
    function_ast: ast::FunctionWithBody,
) -> (Option<(RewriteNode, RewriteNode)>, Vec<PluginDiagnostic>) {
    let mut diagnostics = vec![];
    let declaration = function_ast.declaration(db);
    let signature = declaration.signature(db);
    let ret_ty = declaration.signature(db).ret_ty(db);
    if matches!(ret_ty, OptionReturnTypeClause::ReturnTypeClause(_)) {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: ret_ty.stable_ptr().untyped(),
            message: "Event functions must not return a value.".to_string(),
        });
    }

    let mut param_serializations = Vec::new();
    for param in signature.parameters(db).elements(db) {
        // If we encounter errors with this parameter that don't allow us to serialize it, we skip
        // the serialization of it in the generated code.
        let mut skip_param_serialization = false;
        if is_ref_param(db, &param) {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: param.modifiers(db).stable_ptr().untyped(),
                message: "`ref` parameters are not supported in contract events.".to_string(),
            });
            skip_param_serialization = true;
        }

        let param_name = param.name(db);
        let param_type_ast = param.type_clause(db).ty(db);
        let type_name = param_type_ast.as_syntax_node().get_text(db);
        if skip_param_serialization {
            continue;
        }

        // TODO(yuval): use panicable version of deserializations when supported.
        let param_serialization = RewriteNode::interpolate_patched(
            &format!(
                "serde::Serde::<{type_name}>::serialize(ref data, $param_name$);\n            "
            ),
            HashMap::from([(
                "param_name".to_string(),
                RewriteNode::Trimmed(param_name.as_syntax_node()),
            )]),
        );
        param_serializations.push(param_serialization);
    }

    if !function_ast.body(db).statements(db).elements(db).is_empty() {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: function_ast.body(db).statements(db).stable_ptr().untyped(),
            message: "Event function body must be empty.".to_string(),
        });
    }

    if !diagnostics.is_empty() {
        return (None, diagnostics);
    }

    let name = declaration.name(db).text(db);
    let event_key = format!("0x{:x}", starknet_keccak(name.as_bytes()));

    (
        Some((
            // Event function
            RewriteNode::interpolate_patched(
                &format!(
                    "
        $attrs$
        $declaration$ {{
            let mut keys = array_new();
            array_append(ref keys, {event_key});
            let mut data = array_new();
            $param_serializations$
            starknet::emit_event_syscall(keys, data);
        }}
            "
                ),
                HashMap::from([
                    // TODO(yuval): All the attributes are currently copied. Remove the #[event]
                    // attr.
                    (
                        "attrs".to_string(),
                        RewriteNode::Trimmed(function_ast.attributes(db).as_syntax_node()),
                    ),
                    ("declaration".to_string(), RewriteNode::Trimmed(declaration.as_syntax_node())),
                    (
                        "param_serializations".to_string(),
                        RewriteNode::Modified(ModifiedNode { children: param_serializations }),
                    ),
                ]),
            ),
            // ABI event
            RewriteNode::Modified(ModifiedNode {
                children: vec![
                    RewriteNode::Text("#[event]\n        ".to_string()),
                    RewriteNode::Trimmed(function_ast.declaration(db).as_syntax_node()),
                    RewriteNode::Text(";\n        ".to_string()),
                ],
            }),
        )),
        diagnostics,
    )
}

/// Generate getters and setters for the variables in the storage struct.
fn handle_storage_struct(
    db: &dyn SyntaxGroup,
    struct_ast: ast::ItemStruct,
) -> (RewriteNode, Vec<PluginDiagnostic>) {
    let mut members_code = Vec::new();
    let diagnostics = vec![];

    for member in struct_ast.members(db).elements(db) {
        let name = member.name(db).text(db);
        let address = format!("0x{:x}", starknet_keccak(name.as_bytes()));
        let type_ast = member.type_clause(db).ty(db);
        members_code.push(
            if let Some((key_type_ast, value_type_ast)) = try_extract_mapping_types(db, &type_ast) {
                RewriteNode::interpolate_patched(
                    handle_mapping_storage_var(&address).as_str(),
                    HashMap::from([
                        (
                            "storage_var_name".to_string(),
                            RewriteNode::Trimmed(member.name(db).as_syntax_node()),
                        ),
                        (
                            "key_type".to_string(),
                            RewriteNode::Trimmed(key_type_ast.as_syntax_node()),
                        ),
                        (
                            "value_type".to_string(),
                            RewriteNode::Trimmed(value_type_ast.as_syntax_node()),
                        ),
                    ]),
                )
            } else {
                RewriteNode::interpolate_patched(
                    handle_simple_storage_var(&address).as_str(),
                    HashMap::from([
                        (
                            "storage_var_name".to_string(),
                            RewriteNode::Trimmed(member.name(db).as_syntax_node()),
                        ),
                        ("type_name".to_string(), RewriteNode::Trimmed(type_ast.as_syntax_node())),
                    ]),
                )
            },
        );
    }
    (RewriteNode::Modified(ModifiedNode { children: members_code }), diagnostics)
}

/// Given a type, if it is of form `Map::<K, V>`, returns `K` and `V`. Otherwise, returns None.
fn try_extract_mapping_types(
    db: &dyn SyntaxGroup,
    type_ast: &ast::Expr,
) -> Option<(ast::Expr, ast::Expr)> {
    let as_path = try_extract_matches!(type_ast, ast::Expr::Path)?;
    let [ast::PathSegment::WithGenericArgs(segment)] = &as_path.elements(db)[..] else {
        return None;
    };
    let ty = segment.ident(db).text(db);
    if ty == "Map" {
        let [key_ty, value_ty] =
            <[ast::Expr; 2]>::try_from(segment.generic_args(db).generic_args(db).elements(db))
                .ok()?;
        Some((key_ty, value_ty))
    } else {
        None
    }
}

/// Generate getters and setters skeleton for a non-mapping member in the storage struct.
fn handle_simple_storage_var(address: &str) -> String {
    format!(
        "
    mod $storage_var_name$ {{
        fn address() -> starknet::StorageBaseAddress {{
            starknet::storage_base_address_const::<{address}>()
        }}
        fn read() -> $type_name$ {{
            // Only address_domain 0 is currently supported.
            let address_domain = 0;
            match starknet::StorageAccess::<$type_name$>::read(address_domain, address()) {{
                Result::Ok(value) => value,
                Result::Err(revert_reason) => {{
                    let mut err_data = array_new();
                    array_append(ref err_data, revert_reason);
                    panic(err_data)
                }},
            }}
        }}
        fn write(value: $type_name$) {{
            // Only address_domain 0 is currently supported.
            let address_domain = 0;
            match starknet::StorageAccess::<$type_name$>::write(
                address_domain,
                address(),
                value,
            ) {{
                Result::Ok(()) => {{}},
                Result::Err(revert_reason) => {{
                    let mut err_data = array_new();
                    array_append(ref err_data, revert_reason);
                    panic(err_data)
                }},
            }}
        }}
    }}"
    )
}

/// Generate getters and setters skeleton for a non-mapping member in the storage struct.
fn handle_mapping_storage_var(address: &str) -> String {
    format!(
        "
    mod $storage_var_name$ {{
        fn address(key: $key_type$) -> starknet::StorageBaseAddress {{
            starknet::storage_base_address_from_felt(
                hash::LegacyHash::<$key_type$>::hash({address}, key))
        }}
        fn read(key: $key_type$) -> $value_type$ {{
            // Only address_domain 0 is currently supported.
            let address_domain = 0;
            match starknet::StorageAccess::<$value_type$>::read(address_domain, address(key)) {{
                Result::Ok(value) => value,
                Result::Err(revert_reason) => {{
                    let mut err_data = array_new();
                    array_append(ref err_data, revert_reason);
                    panic(err_data)
                }},
            }}
        }}
        fn write(key: $key_type$, value: $value_type$) {{
            // Only address_domain 0 is currently supported.
            let address_domain = 0;
            match starknet::StorageAccess::<$value_type$>::write(
                address_domain,
                address(key),
                value,
            ) {{
                Result::Ok(()) => {{}},
                Result::Err(revert_reason) => {{
                    let mut err_data = array_new();
                    array_append(ref err_data, revert_reason);
                    panic(err_data)
                }},
            }}
        }}
    }}"
    )
}

/// Generates Cairo code for an entry point wrapper.
fn generate_entry_point_wrapper(
    db: &dyn SyntaxGroup,
    function: &FunctionWithBody,
) -> Result<RewriteNode, Vec<PluginDiagnostic>> {
    let declaration = function.declaration(db);
    let sig = declaration.signature(db);
    let params = sig.parameters(db).elements(db);
    let diagnostics = vec![];
    let mut arg_names = Vec::new();
    let mut arg_definitions = Vec::new();
    let mut ref_appends = Vec::new();
    let input_data_short_err = "'Input too short for arguments'";
    for param in params {
        let arg_name = format!("__arg_{}", param.name(db).text(db));
        let arg_type_ast = param.type_clause(db).ty(db);
        let type_name = arg_type_ast.as_syntax_node().get_text_without_trivia(db);

        let is_ref = is_ref_param(db, &param);
        let ref_modifier = if is_ref { "ref " } else { "" };
        arg_names.push(format!("{ref_modifier}{arg_name}"));
        let mut_modifier = if is_ref { "mut " } else { "" };
        // TODO(yuval): use panicable version of deserializations when supported.
        let arg_definition = format!(
            "
            let {mut_modifier}{arg_name} =
                match serde::Serde::<{type_name}>::deserialize(ref data) {{
                    Option::Some(x) => x,
                    Option::None(()) => {{
                        let mut err_data = array_new();
                        array_append(ref err_data, {input_data_short_err});
                        panic(err_data)
                    }},
                }};"
        );
        arg_definitions.push(arg_definition);

        if is_ref {
            ref_appends.push(RewriteNode::Text(format!(
                "\n            serde::Serde::<{type_name}>::serialize(ref arr, {arg_name});"
            )));
        }
    }
    let arg_names_str = arg_names.join(", ");

    let function_name = RewriteNode::Trimmed(declaration.name(db).as_syntax_node());
    let wrapped_name = RewriteNode::interpolate_patched(
        "super::$function_name$",
        HashMap::from([("function_name".to_string(), function_name.clone())]),
    );
    let (let_res, append_res) = match sig.ret_ty(db) {
        OptionReturnTypeClause::Empty(_) => ("", "".to_string()),
        OptionReturnTypeClause::ReturnTypeClause(ty) => {
            let ret_type_ast = ty.ty(db);
            let ret_type_name = ret_type_ast.as_syntax_node().get_text_without_trivia(db);
            (
                "\n            let res = ",
                format!("\n            serde::Serde::<{ret_type_name}>::serialize(ref arr, res)"),
            )
        }
    };
    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    let oog_err = "'Out of gas'";
    let input_data_long_err = "'Input too long for arguments'";

    let arg_definitions = arg_definitions.join("\n");
    // TODO(yuval): use panicable version of `get_gas` once inlining is supported.
    Ok(RewriteNode::interpolate_patched(
        format!(
            "fn $function_name$(mut data: Array::<felt>) -> Array::<felt> {{
            match get_gas() {{
                Option::Some(_) => {{
                }},
                Option::None(_) => {{
                    let mut err_data = array_new();
                    array_append(ref err_data, {oog_err});
                    panic(err_data);
                }},
            }}
            {arg_definitions}
            if array_len(ref data) != 0_u128 {{
                // Force the inclusion of `System` in the list of implicits.
                starknet::use_system_implicit();

                let mut err_data = array_new();
                array_append(ref err_data, {input_data_long_err});
                panic(err_data);
            }}
            match get_gas_all(get_builtin_costs()) {{
                Option::Some(_) => {{
                }},
                Option::None(_) => {{
                    let mut err_data = array_new();
                    array_append(ref err_data, {oog_err});
                    panic(err_data)
                }},
            }}
            {let_res}$wrapped_name$({arg_names_str});
            let mut arr = array_new();
            // References.$ref_appends$
            // Result.{append_res}
            arr
        }}"
        )
        .as_str(),
        HashMap::from([
            ("function_name".to_string(), function_name),
            ("wrapped_name".to_string(), wrapped_name),
            (
                "ref_appends".to_string(),
                RewriteNode::Modified(ModifiedNode { children: ref_appends }),
            ),
            ("nothing".to_string(), RewriteNode::Text("".to_string())),
        ]),
    ))
}

/// Checks if the parameter is defined as a ref parameter.
fn is_ref_param(db: &dyn SyntaxGroup, param: &Param) -> bool {
    let param_modifiers = param.modifiers(db).elements(db);
    // TODO(yuval): This works only if "ref" is the only modifier. If the expansion was at the
    // semantic level, we could just ask if it's a reference.
    param_modifiers.len() == 1 && matches!(param_modifiers[0], Modifier::Ref(_))
}
