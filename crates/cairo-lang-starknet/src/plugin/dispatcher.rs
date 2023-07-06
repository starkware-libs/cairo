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
use itertools::Itertools;

use super::aux_data::StarkNetABIAuxData;
use super::consts::CALLDATA_PARAM_NAME;
use super::utils::is_ref_param;
use super::{DEPRECATED_ABI_ATTR, INTERFACE_ATTR};
use crate::contract::starknet_keccak;

/// If the trait is annotated with ABI_ATTR, generate the relevant dispatcher logic.
pub fn handle_trait(db: &dyn SyntaxGroup, trait_ast: ast::ItemTrait) -> PluginResult {
    if trait_ast.has_attr(db, DEPRECATED_ABI_ATTR) {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: format!(
                    "The '{DEPRECATED_ABI_ATTR}' attribute was deprecated, please use \
                     `{INTERFACE_ATTR}` instead.",
                ),
                stable_ptr: trait_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    }
    if !trait_ast.has_attr(db, INTERFACE_ATTR) {
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
    let mut safe_dispatcher_signatures = vec![];
    let mut contract_caller_method_impls = vec![];
    let mut library_caller_method_impls = vec![];
    let mut safe_contract_caller_method_impls = vec![];
    let mut safe_library_caller_method_impls = vec![];
    let base_name = trait_ast.name(db).text(db);
    let dispatcher_trait_name = format!("{base_name}DispatcherTrait");
    let safe_dispatcher_trait_name = format!("{base_name}SafeDispatcherTrait");
    let contract_caller_name = format!("{base_name}Dispatcher");
    let safe_contract_caller_name = format!("{base_name}SafeDispatcher");
    let library_caller_name = format!("{base_name}LibraryDispatcher");
    let safe_library_caller_name = format!("{base_name}SafeLibraryDispatcher");
    for item_ast in body.items(db).elements(db) {
        match item_ast {
            ast::TraitItem::Function(func) => {
                let declaration = func.declaration(db);

                let mut skip_generation = false;
                let mut serialization_code = vec![];
                let signature = declaration.signature(db);
                let mut params = signature.parameters(db).elements(db).into_iter();
                // The first parameter is the `self` parameter.
                let Some(self_param) = params.next() else {
                    diagnostics.push(PluginDiagnostic {
                        message: "ABI functions must have a `self` parameter.".to_string(),
                        stable_ptr: declaration.stable_ptr().untyped(),
                    });
                    continue;
                };
                if self_param.name(db).text(db) != "self" {
                    diagnostics.push(PluginDiagnostic {
                        message: "The `self` parameter must be named `self`.".to_string(),
                        stable_ptr: self_param.stable_ptr().untyped(),
                    });
                    skip_generation = true;
                }

                for param in params {
                    if is_ref_param(db, &param) {
                        skip_generation = true;

                        diagnostics.push(PluginDiagnostic {
                            message: "`ref` parameters are not supported in the ABI of a contract."
                                .to_string(),
                            stable_ptr: param.modifiers(db).stable_ptr().untyped(),
                        })
                    }

                    if param.name(db).text(db) == CALLDATA_PARAM_NAME {
                        skip_generation = true;

                        diagnostics.push(PluginDiagnostic {
                            message: "Parameter name `__calldata__` cannot be used.".to_string(),
                            stable_ptr: param.name(db).stable_ptr().untyped(),
                        })
                    }

                    let param_type = param.type_clause(db).ty(db);
                    let type_name = &param_type.as_syntax_node().get_text(db);
                    serialization_code.push(RewriteNode::interpolate_patched(
                        &formatdoc!(
                            "        serde::Serde::<{type_name}>::serialize(@$arg_name$, ref \
                             {CALLDATA_PARAM_NAME});\n"
                        ),
                        [(
                            "arg_name".to_string(),
                            RewriteNode::new_trimmed(param.name(db).as_syntax_node()),
                        )]
                        .into(),
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
                            "\
        option::OptionTrait::expect(
            serde::Serde::<{type_name}>::deserialize(ref ret_data),
            'Returned data too short',
        )"
                        )
                    }
                };
                dispatcher_signatures.push(RewriteNode::interpolate_patched(
                    "$func_decl$;",
                    [("func_decl".to_string(), dispatcher_signature(db, &declaration, "T", true))]
                        .into(),
                ));
                safe_dispatcher_signatures.push(RewriteNode::interpolate_patched(
                    "$func_decl$;",
                    [("func_decl".to_string(), dispatcher_signature(db, &declaration, "T", false))]
                        .into(),
                ));
                let entry_point_selector = RewriteNode::Text(format!(
                    "0x{:x}",
                    starknet_keccak(declaration.name(db).text(db).as_bytes())
                ));
                contract_caller_method_impls.push(declaration_method_impl(
                    dispatcher_signature(db, &declaration, &contract_caller_name, true),
                    entry_point_selector.clone(),
                    "contract_address",
                    "call_contract_syscall",
                    serialization_code.clone(),
                    ret_decode.clone(),
                    true,
                ));
                library_caller_method_impls.push(declaration_method_impl(
                    dispatcher_signature(db, &declaration, &library_caller_name, true),
                    entry_point_selector.clone(),
                    "class_hash",
                    "syscalls::library_call_syscall",
                    serialization_code.clone(),
                    ret_decode.clone(),
                    true,
                ));
                safe_contract_caller_method_impls.push(declaration_method_impl(
                    dispatcher_signature(db, &declaration, &safe_contract_caller_name, false),
                    entry_point_selector.clone(),
                    "contract_address",
                    "call_contract_syscall",
                    serialization_code.clone(),
                    ret_decode.clone(),
                    false,
                ));
                safe_library_caller_method_impls.push(declaration_method_impl(
                    dispatcher_signature(db, &declaration, &safe_library_caller_name, false),
                    entry_point_selector,
                    "class_hash",
                    "syscalls::library_call_syscall",
                    serialization_code,
                    ret_decode,
                    false,
                ));
            }
            // ignore the missing item.
            ast::TraitItem::Missing(_) => {}
        }
    }

    let mut builder = PatchBuilder::new(db);
    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "trait {dispatcher_trait_name}<T> {{
            $dispatcher_signatures$
            }}

            #[derive(Copy, Drop, storage_access::StorageAccess, Serde)]
            struct {contract_caller_name} {{
                contract_address: starknet::ContractAddress,
            }}

            impl {contract_caller_name}Impl of {dispatcher_trait_name}<{contract_caller_name}> {{
            $contract_caller_method_impls$
            }}

            #[derive(Copy, Drop, storage_access::StorageAccess, Serde)]
            struct {library_caller_name} {{
                class_hash: starknet::ClassHash,
            }}

            impl {library_caller_name}Impl of {dispatcher_trait_name}<{library_caller_name}> {{
            $library_caller_method_impls$
            }}

            trait {safe_dispatcher_trait_name}<T> {{
            $safe_dispatcher_signatures$
            }}

            #[derive(Copy, Drop, storage_access::StorageAccess, Serde)]
            struct {safe_library_caller_name} {{
                class_hash: starknet::ClassHash,
            }}

            impl {safe_library_caller_name}Impl of \
             {safe_dispatcher_trait_name}<{safe_library_caller_name}> {{
            $safe_library_caller_method_impls$
            }}


            #[derive(Copy, Drop, storage_access::StorageAccess, Serde)]
            struct {safe_contract_caller_name} {{
                contract_address: starknet::ContractAddress,
            }}

            impl {safe_contract_caller_name}Impl of \
             {safe_dispatcher_trait_name}<{safe_contract_caller_name}> {{
            $safe_contract_caller_method_impls$
            }}
            ",
        ),
        [
            ("dispatcher_signatures".to_string(), RewriteNode::new_modified(dispatcher_signatures)),
            (
                "contract_caller_method_impls".to_string(),
                RewriteNode::new_modified(contract_caller_method_impls),
            ),
            (
                "library_caller_method_impls".to_string(),
                RewriteNode::new_modified(library_caller_method_impls),
            ),
            (
                "safe_dispatcher_signatures".to_string(),
                RewriteNode::new_modified(safe_dispatcher_signatures),
            ),
            (
                "safe_contract_caller_method_impls".to_string(),
                RewriteNode::new_modified(safe_contract_caller_method_impls),
            ),
            (
                "safe_library_caller_method_impls".to_string(),
                RewriteNode::new_modified(safe_library_caller_method_impls),
            ),
        ]
        .into(),
    ));

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: dispatcher_trait_name.into(),
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
    unwrap: bool,
) -> RewriteNode {
    let deserialization_code = if ret_decode.is_empty() {
        RewriteNode::Text("()".to_string())
    } else {
        RewriteNode::Text(if unwrap {
            ret_decode.clone()
        } else {
            ret_decode.split('\n').map(|x| format!("    {x}")).join("\n")
        })
    };
    let return_code = RewriteNode::interpolate_patched(
        if unwrap {
            "let mut ret_data = starknet::SyscallResultTrait::unwrap_syscall(ret_data);
        $deserialization_code$"
        } else if ret_decode.is_empty() {
            "let mut ret_data = ret_data?;
        Result::Ok($deserialization_code$)"
        } else {
            "let mut ret_data = ret_data?;
        Result::Ok(\n        $deserialization_code$\n        )"
        },
        [("deserialization_code".to_string(), deserialization_code)].into(),
    );
    RewriteNode::interpolate_patched(
        &formatdoc!(
            "$func_decl$ {{
                    let mut {CALLDATA_PARAM_NAME} = traits::Default::default();
            $serialization_code$
                    let mut ret_data = starknet::$syscall$(
                        self.$member$,
                        $entry_point_selector$,
                        array::ArrayTrait::span(@{CALLDATA_PARAM_NAME}),
                    );
                    $return_code$
                }}
        "
        ),
        [
            ("func_decl".to_string(), func_declaration),
            ("entry_point_selector".to_string(), entry_point_selector),
            ("syscall".to_string(), RewriteNode::Text(syscall.to_string())),
            ("member".to_string(), RewriteNode::Text(member.to_string())),
            ("serialization_code".to_string(), RewriteNode::new_modified(serialization_code)),
            ("return_code".to_string(), return_code),
        ]
        .into(),
    )
}

/// Returns the matching signature for a dispatcher implementation for the given declaration.
fn dispatcher_signature(
    db: &dyn SyntaxGroup,
    declaration: &ast::FunctionDeclaration,
    self_type_name: &str,
    unwrap: bool,
) -> RewriteNode {
    let mut func_declaration = RewriteNode::from_ast(declaration);
    let params = func_declaration
        .modify_child(db, ast::FunctionDeclaration::INDEX_SIGNATURE)
        .modify_child(db, ast::FunctionSignature::INDEX_PARAMETERS)
        .modify(db)
        .children
        .as_mut()
        .unwrap();
    drop(params.drain(0..std::cmp::min(2, params.len())));
    params.splice(
        0..0,
        [RewriteNode::Text(format!("self: {self_type_name}")), RewriteNode::Text(", ".to_string())],
    );
    if unwrap {
        return func_declaration;
    }
    let return_type = func_declaration
        .modify_child(db, ast::FunctionDeclaration::INDEX_SIGNATURE)
        .modify_child(db, ast::FunctionSignature::INDEX_RET_TY)
        .modify(db)
        .children
        .as_mut()
        .unwrap();

    if return_type.is_empty() {
        let new_ret_type = RewriteNode::Text(String::from(" -> starknet::SyscallResult<()>"));
        return_type.splice(0..0, [new_ret_type]);
    } else {
        let previous_ret_type = RewriteNode::new_modified(return_type[1..2].into());
        let new_ret_type = RewriteNode::interpolate_patched(
            "starknet::SyscallResult<$ret_type$>",
            [("ret_type".to_string(), previous_ret_type)].into(),
        );
        return_type.splice(1..2, [new_ret_type]);
    };

    func_declaration
}
