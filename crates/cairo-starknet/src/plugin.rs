use std::collections::HashMap;
use std::sync::Arc;
use std::vec;

use cairo_defs::plugin::{
    DynGeneratedFileAuxData, GeneratedFileAuxData, MacroPlugin, PluginDiagnostic,
    PluginGeneratedFile, PluginResult,
};
use cairo_diagnostics::DiagnosticEntry;
use cairo_semantic::db::SemanticGroup;
use cairo_semantic::patcher::{ModifiedNode, PatchBuilder, Patches, RewriteNode};
use cairo_semantic::plugin::{
    AsDynGeneratedFileAuxData, AsDynMacroPlugin, DiagnosticMapper, DynDiagnosticMapper,
    PluginMappedDiagnostic, SemanticPlugin, TrivialMapper,
};
use cairo_semantic::SemanticDiagnostic;
use cairo_syntax::node::ast::{
    ItemFreeFunction, MaybeModuleBody, Modifier, OptionReturnTypeClause, Param,
};
use cairo_syntax::node::db::SyntaxGroup;
use cairo_syntax::node::helpers::GetIdentifier;
use cairo_syntax::node::{ast, Terminal, TypedSyntaxNode};
use genco::prelude::*;
use indoc::formatdoc;

use crate::contract::starknet_keccak;

const CONTRACT_ATTR: &str = "contract";
const EXTERNAL_ATTR: &str = "external";
const VIEW_ATTR: &str = "view";
pub const GENERATED_CONTRACT_ATTR: &str = "generated_contract";
pub const ABI_TRAIT: &str = "__abi";
pub const EXTERNAL_MODULE: &str = "__external";

// TODO(yg): this is taken from Ilya's PR.
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
                remove_original_item: false,
            };
        }
    };

    let contract_name = RewriteNode::Copied(module_ast.name(db).token(db).as_syntax_node());
    let mut generated_external_functions = Vec::new();

    let mut storage_code = "".to_string();
    let mut original_items = Vec::new();
    let mut external_declarations = Vec::new();
    for item in body.items(db).elements(db) {
        match &item {
            ast::Item::FreeFunction(item_function)
                if item_function.attributes(db).elements(db).iter().any(|attr| {
                    matches!(attr.attr(db).text(db).as_str(), EXTERNAL_ATTR | VIEW_ATTR)
                }) =>
            {
                // TODO(yg): why is this not formatted well? Also - remove leading and trailing
                // trivia.
                external_declarations
                    .push(RewriteNode::Copied(item_function.declaration(db).as_syntax_node()));

                generated_external_functions.push(generate_entry_point_wrapper(db, item_function));
            }
            ast::Item::Struct(item_struct) if item_struct.name(db).text(db) == "Storage" => {
                storage_code = handle_storage_struct(db, item_struct.clone());
            }
            _ => {}
        };
        // TODO(ygggg): item includes the leading and trailing trivia... Remove them. Add a function
        // that removes the leading and trailing trivia for all nodes?
        original_items.push(RewriteNode::Copied(item.as_syntax_node()));
    }

    let generated_contract_mod = RewriteNode::new_interpolate_patched(
        formatdoc!(
            "
            #[{GENERATED_CONTRACT_ATTR}]
            mod $contract_name$ {{
            $original_items$

                trait {ABI_TRAIT} {{
                    $external_declarations$
                }}

                mod {EXTERNAL_MODULE} {{
                    $generated_external_functions$
                }}
            }}
        "
        )
        .as_str(),
        HashMap::from([
            ("contract_name".to_string(), contract_name),
            (
                "original_items".to_string(),
                RewriteNode::Modified(ModifiedNode { children: original_items }),
            ),
            (
                "external_declarations".to_string(),
                RewriteNode::Modified(ModifiedNode { children: external_declarations }),
            ),
            (
                "generated_external_functions".to_string(),
                RewriteNode::Modified(ModifiedNode { children: generated_external_functions }),
            ),
        ]),
    );

    let mut builder = PatchBuilder::new(db);
    builder.add_modified(generated_contract_mod);
    // TODO(ygg): add storage
    // let contract_code =
    //     format!("{}\n{}", storage_code, generated_contract_mod.to_string().unwrap());

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "contract".into(),
            content: builder.code,
            aux_data: DynGeneratedFileAuxData::new(DynDiagnosticMapper::new(DiagnosticRemapper {
                patches: builder.patches,
            })),
        }),
        diagnostics: vec![],
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

/// Generates Cairo code for an entry point wrapper.
fn generate_entry_point_wrapper(db: &dyn SyntaxGroup, function: &ItemFreeFunction) -> RewriteNode {
    let declaration = function.declaration(db);
    let sig = declaration.signature(db);
    let params = sig.parameters(db).elements(db);

    let mut arg_names = Vec::new();
    let mut arg_definitions = Vec::new();
    let mut ref_appends = Vec::new();
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
        // TODO(yg): can the formatdoc! be removed?
        let arg_definition = formatdoc!(
            "let {mut_modifier}{arg_name} = match {deser_func}(data) {{
                Option::Some(x) => x,
                Option::None(()) => {{
                    let mut err_data = array_new::<felt>();
                    array_append::<felt>(err_data, {input_data_short_err});
                    panic(err_data)
                }},
            }};"
        );
        arg_definitions.push(arg_definition);

        if is_ref {
            ref_appends.push(RewriteNode::Text(format!("{ser_func}(arr, {arg_name});")));
        }
    }
    let arg_names_str = arg_names.join(", ");

    let function_name = RewriteNode::Copied(declaration.name(db).as_syntax_node());
    let wrapped_name = RewriteNode::new_interpolate_patched(
        "super::$function_name$",
        HashMap::from([("function_name".to_string(), function_name.clone())]),
    );
    let (let_res, append_res) = match sig.ret_ty(db) {
        OptionReturnTypeClause::Empty(_) => ("", "".to_string()),
        OptionReturnTypeClause::ReturnTypeClause(ty) => {
            let ret_type_name = ty.ty(db).as_syntax_node().get_text(db);
            ("let res = ", format!("serde::serialize_{ret_type_name}(arr, res)"))
        }
    };

    let oog_err = "'Out of gas'";
    let input_data_long_err = "'Input too long for arguments'";

    // let mut builder = PatchBuilder::new(db);
    // TODO(yuval): use panicable version of `get_gas` once inlining is supported.
    let arg_definitions = arg_definitions.join("\n");
    RewriteNode::new_interpolate_patched(
        formatdoc!(
            "fn $function_name$(mut data: Array::<felt>) -> Array::<felt> {{
                        match get_gas() {{
                            Option::Some(_) => {{
                            }},
                            Option::None(_) => {{
                                let mut err_data = array_new::<felt>();
                                array_append::<felt>(err_data, {oog_err});
                                panic(err_data);
                            }},
                        }}

                        {arg_definitions}
                        if array_len::<felt>(data) != 0_u128 {{
                            // Force the inclusion of `System` in the list of implicits.
                            starknet::use_system_implicit();

                            let mut err_data = array_new::<felt>();
                            array_append::<felt>(err_data, {input_data_long_err});
                            panic(err_data);
                        }}
                        {let_res} $wrapped_name$({arg_names_str});
                        let mut arr = array_new::<felt>();
                        $ref_appends$
                        {append_res}
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
        ]),
    )
}

/// Checks if the parameter is defined as a ref parameter.
fn is_ref_param(db: &dyn SyntaxGroup, param: &Param) -> bool {
    let param_modifiers = param.modifiers(db).elements(db);
    // TODO(yuval): This works only if "ref" is the only modifier. If the expansion was at the
    // semantic level, we could just ask if it's a reference.
    param_modifiers.len() == 1 && matches!(param_modifiers[0], Modifier::Ref(_))
}
