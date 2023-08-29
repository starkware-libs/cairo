use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::{PluginDiagnostic, PluginResult};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, PathSegmentEx, QueryAttrs};
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use indoc::{formatdoc, indoc};

use super::generation_data::{ContractGenerationData, StarknetModuleCommonGenerationData};
use super::{grand_grand_parent_starknet_module, StarknetModuleKind};
use crate::contract::starknet_keccak;
use crate::plugin::consts::{
    COMPONENT_INLINE_MACRO, CONSTRUCTOR_ATTR, CONTRACT_STATE_NAME, EMBED_ATTR, EXTERNAL_ATTR,
    L1_HANDLER_ATTR, STORAGE_STRUCT_NAME,
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
    has_component_impls: Vec<RewriteNode>,
}
impl ContractSpecificGenerationData {
    pub fn into_rewrite_node(self) -> RewriteNode {
        RewriteNode::interpolate_patched(
            indoc! {"
                $test_config$
                $entry_points_code$
                $has_component_impls$"},
            [
                ("test_config".to_string(), self.test_config),
                ("entry_points_code".to_string(), self.entry_points_code.into_rewrite_node()),
                (
                    "has_component_impls".to_string(),
                    RewriteNode::new_modified(self.has_component_impls),
                ),
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
        ast::Item::InlineMacro(inline_macro_ast)
            if inline_macro_ast.name(db).text(db) == COMPONENT_INLINE_MACRO =>
        {
            handle_component_inline_macro(db, diagnostics, inline_macro_ast, &mut data.specific)
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

/// Handles an embedded impl by an impl alias.
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

/// Handles a `component!` inline macro. Assumes that the macro name is `COMPONENT_INLINE_MACRO`.
/// Verifies that the macro pattern is:
/// component!(name: <component_name>, storage: <storage_name>, event: <event_name>);
/// If the macro pattern is as expected, generates the code for impl of HasComponent in the
/// contract.
pub fn handle_component_inline_macro(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    component_macro_ast: &ast::ItemInlineMacro,
    data: &mut ContractSpecificGenerationData,
) {
    let macro_args = match component_macro_ast.arguments(db) {
        ast::WrappedArgList::ParenthesizedArgList(args) => args.args(db),
        _ => {
            diagnostics.push(invalid_macro_diagnostic(component_macro_ast));
            return;
        }
    };
    let arguments = macro_args.elements(db);
    let [path_arg, storage_arg, event_arg] = arguments.as_slice() else {
        diagnostics.push(invalid_macro_diagnostic(component_macro_ast));
        return;
    };

    let (Some(component_path), Some(_storage_name), Some(event_name)) = (
        try_extract_named_macro_argument(db, diagnostics, path_arg, "path", false),
        // TODO(yuval): use storage_name. Currently we only verify it is set correctly.
        try_extract_named_macro_argument(db, diagnostics, storage_arg, "storage", true),
        try_extract_named_macro_argument(db, diagnostics, event_arg, "event", true),
    ) else {
        return;
    };

    // TODO(yuval): consider supporting 2 components with the same name and different paths.
    // Currently it doesn't work as the name of the impl is the same.
    let component_name = match component_path.elements(db).last().unwrap() {
        ast::PathSegment::WithGenericArgs(x) => x.ident(db),
        ast::PathSegment::Simple(x) => x.ident(db),
    };

    let has_component_impl = RewriteNode::interpolate_patched(
        indoc!(
            "impl HasComponentImpl_$component_name$ of \
             $component_path$::HasComponent<ContractState> {
           fn get_component(self: @ContractState) -> \
             @$component_path$::ComponentState<ContractState> {
               @$component_path$::unsafe_new_component_state::<ContractState>()
           }
           fn get_component_mut(ref self: ContractState) -> \
             $component_path$::ComponentState<ContractState> {
               $component_path$::unsafe_new_component_state::<ContractState>()
           }
           fn get_contract(self: @$component_path$::ComponentState<ContractState>) -> \
             @ContractState {
               @unsafe_new_contract_state()
           }
           fn get_contract_mut(ref self: $component_path$::ComponentState<ContractState>) -> \
             ContractState {
               unsafe_new_contract_state()
           }
           fn emit(ref self: $component_path$::ComponentState<ContractState>, event: \
             $component_path$::Event) {
               let mut contract = $component_path$::HasComponent::get_contract_mut(ref self);
               contract.emit(Event::$event_name$(event));
           }
       }"
        ),
        [
            (
                "component_name".to_string(),
                RewriteNode::new_trimmed(component_name.as_syntax_node()),
            ),
            (
                "component_path".to_string(),
                RewriteNode::new_trimmed(component_path.as_syntax_node()),
            ),
            ("event_name".to_string(), RewriteNode::new_trimmed(event_name.as_syntax_node())),
        ]
        .into(),
    );

    data.has_component_impls.push(has_component_impl);
}

/// Returns an invalid `component` macro diagnostic.
fn invalid_macro_diagnostic(component_macro_ast: &ast::ItemInlineMacro) -> PluginDiagnostic {
    PluginDiagnostic {
        message: "Invalid component macro, expected `component!(name: \"<component_name>\", \
                  storage: \"<storage_name>\", event: \"<event_name>\");`"
            .to_string(),
        stable_ptr: component_macro_ast.stable_ptr().untyped(),
    }
}

/// Remove a `component!` inline macro from the original code if it's inside a starknet::contract.
/// Assumes that the macro name is `COMPONENT_INLINE_MACRO`.
pub fn remove_component_inline_macro(
    db: &dyn SyntaxGroup,
    component_macro_ast: &ast::ItemInlineMacro,
) -> PluginResult {
    if let Some((_module_ast, module_kind)) =
        grand_grand_parent_starknet_module(component_macro_ast.as_syntax_node(), db)
    {
        if module_kind == StarknetModuleKind::Contract {
            return PluginResult { code: None, diagnostics: vec![], remove_original_item: true };
        }
    }
    PluginResult { code: None, diagnostics: vec![], remove_original_item: false }
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

/// Verifies that a given Arg is named with given name and that the value is a path (simple if
/// `only_simple_identifier` is true). Returns the value path if the verification succeeds,
/// otherwise returns None.
fn try_extract_named_macro_argument(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    arg_ast: &ast::Arg,
    arg_name: &str,
    only_simple_identifier: bool,
) -> Option<ast::ExprPath> {
    match arg_ast.arg_clause(db) {
        ast::ArgClause::Named(clause) if clause.name(db).text(db) == arg_name => {
            match clause.value(db) {
                ast::Expr::Path(path) => {
                    if !only_simple_identifier {
                        return Some(path);
                    }
                    let elements = path.elements(db);
                    if elements.len() != 1
                        || !matches!(elements.last().unwrap(), ast::PathSegment::Simple(_))
                    {
                        diagnostics.push(PluginDiagnostic {
                            message: format!(
                                "Component macro argument `{arg_name}` must be a simple \
                                 identifier.",
                            ),
                            stable_ptr: path.stable_ptr().untyped(),
                        });
                        return None;
                    }
                    Some(path)
                }
                value => {
                    diagnostics.push(PluginDiagnostic {
                        message: format!(
                            "Component macro argument `{arg_name}` must be a path expression.",
                        ),
                        stable_ptr: value.stable_ptr().untyped(),
                    });
                    None
                }
            }
        }
        _ => {
            diagnostics.push(PluginDiagnostic {
                message: format!(
                    "Invalid component macro argument. Expected `{0}: <value>`",
                    arg_name
                ),
                stable_ptr: arg_ast.stable_ptr().untyped(),
            });
            None
        }
    }
}
