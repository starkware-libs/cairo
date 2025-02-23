use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{PluginDiagnostic, PluginGeneratedFile, PluginResult};
use cairo_lang_syntax::node::ast::{
    self, MaybeTraitBody, OptionReturnTypeClause, OptionTypeClause,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{BodyItems, QueryAttrs};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::extract_matches;
use indoc::formatdoc;
use itertools::Itertools;

use super::consts::CALLDATA_PARAM_NAME;
use super::utils::{AstPathExtract, ParamEx};
use super::{DEPRECATED_ABI_ATTR, INTERFACE_ATTR, STORE_TRAIT};

/// The name of the variable that holds the returned data.
const RET_DATA: &str = "__dispatcher_return_data__";

/// If the trait is annotated with INTERFACE_ATTR, generate the relevant dispatcher logic.
pub fn handle_trait(db: &dyn SyntaxGroup, trait_ast: ast::ItemTrait) -> PluginResult {
    if trait_ast.has_attr(db, DEPRECATED_ABI_ATTR) {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic::error(
                trait_ast.stable_ptr().untyped(),
                format!(
                    "The '{DEPRECATED_ABI_ATTR}' attribute for traits was deprecated, please use \
                     `{INTERFACE_ATTR}` instead.",
                ),
            )],
            remove_original_item: false,
        };
    }
    let Some(interface_attr) = trait_ast.find_attr(db, INTERFACE_ATTR) else {
        return PluginResult::default();
    };
    let body = match trait_ast.body(db) {
        MaybeTraitBody::Some(body) => body,
        MaybeTraitBody::None(empty_body) => {
            return PluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic::error(
                    empty_body.stable_ptr().untyped(),
                    "Starknet interfaces without body are not supported.".to_string(),
                )],
                remove_original_item: false,
            };
        }
    };
    let generic_params = trait_ast.generic_params(db);
    let Some(single_generic_param) = (match &generic_params {
        ast::OptionWrappedGenericParamList::Empty(_) => None,
        ast::OptionWrappedGenericParamList::WrappedGenericParamList(generic_params) => {
            if let [ast::GenericParam::Type(param)] =
                generic_params.generic_params(db).elements(db).as_slice()
            {
                Some(param.name(db).text(db))
            } else {
                None
            }
        }
    }) else {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic::error(
                generic_params.stable_ptr().untyped(),
                "Starknet interfaces must have exactly one generic parameter, which is a type."
                    .to_string(),
            )],
            remove_original_item: false,
        };
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
    for item_ast in body.items_vec(db) {
        match item_ast {
            ast::TraitItem::Function(func) => {
                let declaration = func.declaration(db);

                let mut skip_generation = false;
                let mut serialization_code = vec![];
                let signature = declaration.signature(db);
                let mut params = signature.parameters(db).elements(db).into_iter();
                // The first parameter is the `self` parameter.
                let Some(self_param) = params.next() else {
                    diagnostics.push(PluginDiagnostic::error(
                        declaration.stable_ptr().untyped(),
                        "`starknet::interface` functions must have a `self` parameter.".to_string(),
                    ));
                    continue;
                };
                if self_param.name(db).text(db) != "self" {
                    diagnostics.push(PluginDiagnostic::error(
                        self_param.stable_ptr().untyped(),
                        "The first parameter must be named `self`.".to_string(),
                    ));
                    skip_generation = true;
                }
                let self_param_type_ok = if self_param.is_ref_param(db) {
                    extract_matches!(self_param.type_clause(db), OptionTypeClause::TypeClause)
                        .ty(db)
                        .is_identifier(db, &single_generic_param)
                } else if let Some(snapped_ty) = self_param.try_extract_snapshot(db) {
                    snapped_ty.is_identifier(db, &single_generic_param)
                } else {
                    false
                };
                if !self_param_type_ok {
                    diagnostics.push(PluginDiagnostic::error(
                        self_param.stable_ptr().untyped(),
                        "`starknet::interface` function first parameter must be a reference to \
                         the trait's generic parameter or a snapshot of it."
                            .to_string(),
                    ));
                    skip_generation = true;
                }

                for param in params {
                    if param.is_ref_param(db) {
                        skip_generation = true;

                        diagnostics.push(PluginDiagnostic::error(
                            param.modifiers(db).stable_ptr().untyped(),
                            "`starknet::interface` functions don't support `ref` parameters other \
                             than the first one."
                                .to_string(),
                        ))
                    }
                    if extract_matches!(param.type_clause(db), OptionTypeClause::TypeClause)
                        .ty(db)
                        .is_dependent_type(db, &single_generic_param)
                    {
                        skip_generation = true;

                        diagnostics.push(PluginDiagnostic::error(
                            extract_matches!(param.type_clause(db), OptionTypeClause::TypeClause)
                                .ty(db)
                                .stable_ptr()
                                .untyped(),
                            "`starknet::interface` functions don't support parameters that depend \
                             on the trait's generic param type."
                                .to_string(),
                        ))
                    }

                    if param.name(db).text(db) == CALLDATA_PARAM_NAME {
                        skip_generation = true;

                        diagnostics.push(PluginDiagnostic::error(
                            param.name(db).stable_ptr().untyped(),
                            "Parameter name `__calldata__` cannot be used.".to_string(),
                        ))
                    }

                    let param_type =
                        extract_matches!(param.type_clause(db), OptionTypeClause::TypeClause)
                            .ty(db);
                    let type_name = &param_type.as_syntax_node().get_text(db);
                    serialization_code.push(RewriteNode::interpolate_patched(
                        &formatdoc!(
                            "        core::serde::Serde::<{type_name}>::serialize(@$arg_name$, \
                             ref {CALLDATA_PARAM_NAME});\n"
                        ),
                        &[("arg_name".to_string(), RewriteNode::from_ast_trimmed(&param.name(db)))]
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
        core::option::OptionTrait::expect(
            core::serde::Serde::<{type_name}>::deserialize(ref {RET_DATA}),
            'Returned data too short',
        )"
                        )
                    }
                };
                dispatcher_signatures.push(RewriteNode::interpolate_patched(
                    "\n$func_decl$;",
                    &[("func_decl".to_string(), dispatcher_signature(db, &declaration, "T", true))]
                        .into(),
                ));
                safe_dispatcher_signatures.push(RewriteNode::interpolate_patched(
                    "\n    #[unstable(feature: \"safe_dispatcher\")]\n$func_decl$;",
                    &[(
                        "func_decl".to_string(),
                        dispatcher_signature(db, &declaration, "T", false),
                    )]
                    .into(),
                ));
                let entry_point_selector =
                    RewriteNode::Text(format!("selector!(\"{}\")", declaration.name(db).text(db)));
                contract_caller_method_impls.push(declaration_method_impl(
                    dispatcher_signature(db, &declaration, &contract_caller_name, true),
                    entry_point_selector.clone(),
                    "contract_address",
                    "syscalls::call_contract_syscall",
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
                    "syscalls::call_contract_syscall",
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
            // Ignore the missing item.
            ast::TraitItem::Missing(_) => {}
            ast::TraitItem::Type(ty) => {
                diagnostics.push(PluginDiagnostic::error(
                    ty.type_kw(db).stable_ptr().untyped(),
                    "`starknet::interface` does not yet support type items.".to_string(),
                ));
                continue;
            }
            ast::TraitItem::Constant(constant) => {
                diagnostics.push(PluginDiagnostic::error(
                    constant.const_kw(db).stable_ptr().untyped(),
                    "`starknet::interface` does not yet support constant items.".to_string(),
                ));
                continue;
            }
            ast::TraitItem::Impl(imp) => {
                diagnostics.push(PluginDiagnostic::error(
                    imp.impl_kw(db).stable_ptr().untyped(),
                    "`starknet::interface` does not yet support impl items.".to_string(),
                ));
                continue;
            }
        }
    }

    let mut builder = PatchBuilder::new(db, &interface_attr);
    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "$visibility$trait {dispatcher_trait_name}<T> {{$dispatcher_signatures$
            }}

            #[derive(Copy, Drop, {STORE_TRAIT}, Serde)]
            $visibility$struct {contract_caller_name} {{
                pub contract_address: starknet::ContractAddress,
            }}

            impl {contract_caller_name}Impl of {dispatcher_trait_name}<{contract_caller_name}> {{
            $contract_caller_method_impls$
            }}

            #[derive(Copy, Drop, {STORE_TRAIT}, Serde)]
            $visibility$struct {library_caller_name} {{
                pub class_hash: starknet::ClassHash,
            }}

            impl {library_caller_name}Impl of {dispatcher_trait_name}<{library_caller_name}> {{
            $library_caller_method_impls$
            }}

            $visibility$trait {safe_dispatcher_trait_name}<T> {{$safe_dispatcher_signatures$
            }}

            #[derive(Copy, Drop, {STORE_TRAIT}, Serde)]
            $visibility$struct {safe_library_caller_name} {{
                pub class_hash: starknet::ClassHash,
            }}

            impl {safe_library_caller_name}Impl of \
             {safe_dispatcher_trait_name}<{safe_library_caller_name}> {{
            $safe_library_caller_method_impls$
            }}


            #[derive(Copy, Drop, {STORE_TRAIT}, Serde)]
            $visibility$struct {safe_contract_caller_name} {{
                pub contract_address: starknet::ContractAddress,
            }}

            impl {safe_contract_caller_name}Impl of \
             {safe_dispatcher_trait_name}<{safe_contract_caller_name}> {{
            $safe_contract_caller_method_impls$
            }}
            ",
        ),
        &[
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
            (
                "visibility".to_string(),
                RewriteNode::Copied(trait_ast.visibility(db).as_syntax_node()),
            ),
        ]
        .into(),
    ));

    let (content, code_mappings) = builder.build();
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: dispatcher_trait_name.into(),
            content,
            code_mappings,
            aux_data: None,
            diagnostics_note: Default::default(),
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
        RewriteNode::text("()")
    } else {
        RewriteNode::Text(if unwrap {
            ret_decode.clone()
        } else {
            ret_decode.split('\n').map(|x| format!("    {x}")).join("\n")
        })
    };
    let return_code = RewriteNode::interpolate_patched(
        &if unwrap {
            format!(
                "let mut {RET_DATA} = starknet::SyscallResultTrait::unwrap_syscall({RET_DATA});
        $deserialization_code$"
            )
        } else if ret_decode.is_empty() {
            format!(
                "let mut {RET_DATA} = {RET_DATA}?;
        Result::Ok($deserialization_code$)"
            )
        } else {
            format!(
                "let mut {RET_DATA} = {RET_DATA}?;
        Result::Ok(\n        $deserialization_code$\n        )"
            )
        },
        &[("deserialization_code".to_string(), deserialization_code)].into(),
    );
    RewriteNode::interpolate_patched(
        &formatdoc!(
            "$func_decl$ {{
                    let mut {CALLDATA_PARAM_NAME} = core::traits::Default::default();
            $serialization_code$
                    let mut {RET_DATA} = starknet::$syscall$(
                        self.$member$,
                        $entry_point_selector$,
                        core::array::ArrayTrait::span(@{CALLDATA_PARAM_NAME}),
                    );
                    $return_code$
                }}
        "
        ),
        &[
            ("func_decl".to_string(), func_declaration),
            ("entry_point_selector".to_string(), entry_point_selector),
            ("syscall".to_string(), RewriteNode::text(syscall)),
            ("member".to_string(), RewriteNode::text(member)),
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
    let maybe_comma = if params.len() > 2 { ", " } else { "" };
    params.splice(
        0..std::cmp::min(2, params.len()),
        [RewriteNode::Text(format!("self: {self_type_name}{maybe_comma}"))],
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
        let new_ret_type = RewriteNode::text(" -> starknet::SyscallResult<()>");
        return_type.splice(0..0, [new_ret_type]);
    } else {
        let previous_ret_type = RewriteNode::new_modified(return_type[1..2].into());
        let new_ret_type = RewriteNode::interpolate_patched(
            "starknet::SyscallResult<$ret_type$>",
            &[("ret_type".to_string(), previous_ret_type)].into(),
        );
        return_type.splice(1..2, [new_ret_type]);
    };

    func_declaration
}
