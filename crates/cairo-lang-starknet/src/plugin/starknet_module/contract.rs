use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, PathSegmentEx, QueryAttrs};
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use indoc::{formatdoc, indoc};

use super::generation_data::{ContractGenerationData, StarknetModuleCommonGenerationData};
use super::StarknetModuleKind;
use crate::contract::starknet_keccak;
use crate::plugin::consts::{
    CONSTRUCTOR_ATTR, CONTRACT_STATE_NAME, EMBED_ATTR, EXTERNAL_ATTR, L1_HANDLER_ATTR,
    STORAGE_STRUCT_NAME,
};
use crate::plugin::entry_point::{
    handle_entry_point, has_embed_attribute, has_external_attribute, EntryPointGenerationParams,
    EntryPointKind, EntryPointsGenerationData,
};
use crate::plugin::storage::handle_storage_struct;

/// Accumulated data specific for contract generation.
#[derive(Default)]
pub struct ContractSpecificGenerationData {
    test_config: RewriteNode,
    entry_points_code: EntryPointsGenerationData,
}
impl ContractSpecificGenerationData {
    pub fn into_rewrite_node(self) -> RewriteNode {
        RewriteNode::interpolate_patched(
            indoc! {"
                $test_config$
                $entry_points_code$"},
            [
                ("test_config".to_string(), self.test_config),
                ("entry_points_code".to_string(), self.entry_points_code.into_rewrite_node()),
            ]
            .into(),
        )
    }
}

/// Handles a single item inside a contract module.
fn handle_contract_item(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
    data: &mut ContractGenerationData,
) {
    match item {
        ast::Item::FreeFunction(item_function) => {
            handle_contract_free_function(
                db,
                diagnostics,
                item_function,
                &mut data.specific.entry_points_code,
            );
        }
        ast::Item::Impl(item_impl) => {
            handle_contract_impl(
                db,
                diagnostics,
                item,
                item_impl,
                &mut data.specific.entry_points_code,
            );
        }
        ast::Item::Struct(item_struct) if item_struct.name(db).text(db) == STORAGE_STRUCT_NAME => {
            handle_storage_struct(
                db,
                diagnostics,
                item_struct.clone(),
                StarknetModuleKind::Contract,
                &mut data.common,
            );
        }
        ast::Item::ImplAlias(alias_ast) if alias_ast.has_attr(db, EMBED_ATTR) => {
            handle_embed_impl_alias(
                db,
                diagnostics,
                alias_ast,
                &mut data.specific.entry_points_code,
            );
        }
        _ => {}
    }
}

/// Generates the code that is specific for a contract.
pub(super) fn generate_contract_specific_code(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    common_data: StarknetModuleCommonGenerationData,
    body: &ast::ModuleBody,
    module_ast: &ast::ItemModule,
) -> RewriteNode {
    let mut generation_data = ContractGenerationData { common: common_data, ..Default::default() };
    for item in body.items(db).elements(db) {
        handle_contract_item(db, diagnostics, &item, &mut generation_data);
    }

    let test_class_hash = format!(
        "0x{:x}",
        starknet_keccak(
            module_ast.as_syntax_node().get_text_without_trivia(db).as_str().as_bytes(),
        )
    );

    generation_data.specific.test_config = RewriteNode::Text(formatdoc!(
        "#[cfg(test)]
            const TEST_CLASS_HASH: felt252 = {test_class_hash};
"
    ));

    generation_data.into_rewrite_node()
}

/// Forbids the given attribute in the given impl, assuming it's external.
fn forbid_attribute_in_external_impl(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    impl_item: &ast::ImplItem,
    attr_name: &str,
) {
    if let Some(attr) = impl_item.find_attr(db, attr_name) {
        diagnostics.push(PluginDiagnostic {
            message: format!("The '{attr_name}' attribute is not allowed inside an external impl."),
            stable_ptr: attr.stable_ptr().untyped(),
        });
    }
}

/// Handles a contract entrypoint function.
fn handle_contract_entry_point(
    entry_point_kind: EntryPointKind,
    item_function: &ast::FunctionWithBody,
    wrapped_function_path: RewriteNode,
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    data: &mut EntryPointsGenerationData,
) {
    handle_entry_point(
        db,
        EntryPointGenerationParams {
            entry_point_kind,
            item_function,
            wrapped_function_path,
            unsafe_new_contract_state_prefix: "",
            generic_params: RewriteNode::empty(),
        },
        diagnostics,
        data,
    )
}

/// Handles a free function inside a contract module.
fn handle_contract_free_function(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item_function: &ast::FunctionWithBody,
    data: &mut EntryPointsGenerationData,
) {
    let Some(entry_point_kind) =
        EntryPointKind::try_from_function_with_body(db, diagnostics, item_function)
    else {
        return;
    };
    let function_name =
        RewriteNode::new_trimmed(item_function.declaration(db).name(db).as_syntax_node());
    handle_contract_entry_point(
        entry_point_kind,
        item_function,
        function_name,
        db,
        diagnostics,
        data,
    );
}

/// Handles an impl inside a contract module.
fn handle_contract_impl(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
    item_impl: &ast::ItemImpl,
    data: &mut EntryPointsGenerationData,
) {
    let is_external = has_external_attribute(db, diagnostics, item);
    if !(is_external || has_embed_attribute(db, diagnostics, item)) {
        return;
    }
    let ast::MaybeImplBody::Some(impl_body) = item_impl.body(db) else {
        return;
    };
    let impl_name = RewriteNode::new_trimmed(item_impl.name(db).as_syntax_node());
    for item in impl_body.items(db).elements(db) {
        if is_external {
            for attr in [EXTERNAL_ATTR, CONSTRUCTOR_ATTR, L1_HANDLER_ATTR] {
                forbid_attribute_in_external_impl(db, diagnostics, &item, attr);
            }
        }

        let ast::ImplItem::Function(item_function) = item else {
            continue;
        };
        let entry_point_kind = if is_external {
            EntryPointKind::External
        } else if let Some(entry_point_kind) = EntryPointKind::try_from_attrs(db, &item_function) {
            entry_point_kind
        } else {
            continue;
        };
        let function_name =
            RewriteNode::new_trimmed(item_function.declaration(db).name(db).as_syntax_node());
        let function_name = RewriteNode::interpolate_patched(
            "$impl_name$::$func_name$",
            [
                ("impl_name".to_string(), impl_name.clone()),
                ("func_name".to_string(), function_name),
            ]
            .into(),
        );
        handle_contract_entry_point(
            entry_point_kind,
            &item_function,
            function_name,
            db,
            diagnostics,
            data,
        );
    }
}

/// Handles an ebmedded impl by impl alias.
fn handle_embed_impl_alias(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    alias_ast: &ast::ItemImplAlias,
    data: &mut EntryPointsGenerationData,
) {
    let has_generic_params = match alias_ast.generic_params(db) {
        ast::OptionWrappedGenericParamList::Empty(_) => false,
        ast::OptionWrappedGenericParamList::WrappedGenericParamList(generics) => {
            !generics.generic_params(db).elements(db).is_empty()
        }
    };
    if has_generic_params {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: alias_ast.stable_ptr().untyped(),
            message: "Generic parameters are not supported in impl aliases with `#[embed]`."
                .to_string(),
        });
        return;
    }
    let elements = alias_ast.impl_path(db).elements(db);
    let Some((impl_final_part, impl_module)) = elements.split_last() else {
        unreachable!("impl_path should have at least one segment")
    };

    if !is_first_generic_arg_contract_state(db, impl_final_part) {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: alias_ast.stable_ptr().untyped(),
            message: "First generic argument of impl alias with `#[embed]` must be \
                      `ContractState`."
                .to_string(),
        });
        return;
    }
    let impl_name = impl_final_part.identifier_ast(db);
    let impl_module = RewriteNode::new_modified(
        impl_module
            .iter()
            .flat_map(|segment| {
                vec![
                    RewriteNode::new_trimmed(segment.as_syntax_node()),
                    RewriteNode::Text("::".to_string()),
                ]
            })
            .collect(),
    );
    data.generated_wrapper_functions.push(RewriteNode::interpolate_patched(
        formatdoc! {"
        impl ContractState$impl_name$ of
            $impl_module$UnsafeNewContractStateTraitFor$impl_name$<{CONTRACT_STATE_NAME}> {{
            fn unsafe_new_contract_state() -> {CONTRACT_STATE_NAME} {{
                unsafe_new_contract_state()
            }}
        }}
    "}
        .as_str(),
        [
            ("impl_name".to_string(), RewriteNode::new_trimmed(impl_name.as_syntax_node())),
            ("impl_module".to_string(), impl_module),
        ]
        .into(),
    ));
}

/// Checks whether the first generic argument in the path segment is `CONTRACT_STATE_NAME`.
fn is_first_generic_arg_contract_state(
    db: &dyn SyntaxGroup,
    final_path_segment: &ast::PathSegment,
) -> bool {
    let Some(generic_args) = final_path_segment.generic_args(db) else {
        return false;
    };
    let Some(ast::GenericArg::Unnamed(first_generic_arg)) = generic_args.first() else {
        return false;
    };
    let ast::GenericArgValue::Expr(first_generic_arg) = first_generic_arg.value(db) else {
        return false;
    };
    let ast::Expr::Path(first_generic_arg) = first_generic_arg.expr(db) else {
        return false;
    };
    first_generic_arg.identifier(db) == CONTRACT_STATE_NAME
}
