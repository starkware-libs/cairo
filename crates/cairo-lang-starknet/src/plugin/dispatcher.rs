use std::collections::HashMap;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_semantic::plugin::DynPluginAuxData;
use cairo_lang_syntax::node::ast::{self, MaybeTraitBody, OptionReturnTypeClause};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};
use indoc::formatdoc;

use super::aux_data::StarkNetABIAuxData;
use super::consts::EVENT_ATTR;
use super::utils::is_ref_param;
use super::ABI_ATTR;
use crate::contract::starknet_keccak;

/// If the trait is annotated with ABI_ATTR, generate the relevant dispatcher logic.
pub fn handle_trait(db: &dyn SyntaxGroup, trait_ast: ast::ItemTrait) -> PluginResult {
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
    let mut dispatcher_signatures = vec![];
    let mut contract_caller_method_impls = vec![];
    let mut library_caller_method_impls = vec![];
    let base_name = trait_ast.name(db).text(db);
    let dispatcher_name = format!("{base_name}DispatcherTrait");
    let contract_caller_name = format!("{base_name}Dispatcher");
    let library_caller_name = format!("{base_name}LibraryDispatcher");
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
                            RewriteNode::new_trimmed(param.name(db).as_syntax_node()),
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
                        format!(
                            "
        option::OptionTrait::expect(
            serde::Serde::<{type_name}>::deserialize(ref ret_data),
            'Returned data too short',
        )"
                        )
                    }
                };
                dispatcher_signatures.push(RewriteNode::interpolate_patched(
                    "$func_decl$;",
                    HashMap::from([(
                        "func_decl".to_string(),
                        dispatcher_signature(db, &declaration, "T"),
                    )]),
                ));
                let entry_point_selector = RewriteNode::Text(format!(
                    "0x{:x}",
                    starknet_keccak(declaration.name(db).text(db).as_bytes())
                ));
                contract_caller_method_impls.push(declaration_method_impl(
                    dispatcher_signature(db, &declaration, &contract_caller_name),
                    entry_point_selector.clone(),
                    "contract_address",
                    "call_contract_syscall",
                    serialization_code.clone(),
                    ret_decode.clone(),
                ));
                library_caller_method_impls.push(declaration_method_impl(
                    dispatcher_signature(db, &declaration, &library_caller_name),
                    entry_point_selector,
                    "class_hash",
                    "syscalls::library_call_syscall",
                    serialization_code,
                    ret_decode,
                ));
            }
        }
    }

    let mut builder = PatchBuilder::new(db);
    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "trait {dispatcher_name}<T> {{
            $dispatcher_signatures$
            }}

            #[derive(Copy, Drop)]
            struct {contract_caller_name} {{
                contract_address: starknet::ContractAddress,
            }}

            impl {contract_caller_name}Impl of {dispatcher_name}::<{contract_caller_name}> {{
            $contract_caller_method_impls$
            }}

            #[derive(Copy, Drop)]
            struct {library_caller_name} {{
                class_hash: starknet::ClassHash,
            }}

            impl {library_caller_name}Impl of {dispatcher_name}::<{library_caller_name}> {{
            $library_caller_method_impls$
            }}",
        ),
        HashMap::from([
            ("dispatcher_signatures".to_string(), RewriteNode::new_modified(dispatcher_signatures)),
            (
                "contract_caller_method_impls".to_string(),
                RewriteNode::new_modified(contract_caller_method_impls),
            ),
            (
                "library_caller_method_impls".to_string(),
                RewriteNode::new_modified(library_caller_method_impls),
            ),
        ]),
    ));
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: dispatcher_name.into(),
            content: builder.code,
            aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(StarkNetABIAuxData {
                patches: builder.patches,
            })),
        }),
        diagnostics,
        remove_original_item: false,
    }
}

/// Returns the method implementation rewrite node for a declaration.
fn declaration_method_impl(
    func_declaration: RewriteNode,
    entry_point_selector: RewriteNode,
    member: &str,
    syscall: &str,
    serialization_code: Vec<RewriteNode>,
    ret_decode: String,
) -> RewriteNode {
    RewriteNode::interpolate_patched(
        "$func_decl$ {
        let mut calldata = array::ArrayTrait::new();
$serialization_code$
        let mut ret_data = array::ArrayTrait::span(
            @starknet::SyscallResultTrait::unwrap_syscall(
                starknet::$syscall$(
                    self.$member$,
                    $entry_point_selector$,
                    calldata,
                )
            )
        );
$deserialization_code$
    }
",
        HashMap::from([
            ("func_decl".to_string(), func_declaration),
            ("entry_point_selector".to_string(), entry_point_selector),
            ("syscall".to_string(), RewriteNode::Text(syscall.to_string())),
            ("member".to_string(), RewriteNode::Text(member.to_string())),
            ("serialization_code".to_string(), RewriteNode::new_modified(serialization_code)),
            ("deserialization_code".to_string(), RewriteNode::Text(ret_decode)),
        ]),
    )
}

/// Returns the matching signature for a dispatcher implementation for the given declaration.
fn dispatcher_signature(
    db: &dyn SyntaxGroup,
    declaration: &ast::FunctionDeclaration,
    self_type_name: &str,
) -> RewriteNode {
    let mut func_declaration = RewriteNode::from_ast(declaration);
    func_declaration
        .modify_child(db, ast::FunctionDeclaration::INDEX_SIGNATURE)
        .modify_child(db, ast::FunctionSignature::INDEX_PARAMETERS)
        .modify(db)
        .children
        .as_mut()
        .unwrap()
        .splice(
            0..0,
            [
                RewriteNode::Text(format!("self: {self_type_name}")),
                RewriteNode::Text(", ".to_string()),
            ],
        );
    func_declaration
}
