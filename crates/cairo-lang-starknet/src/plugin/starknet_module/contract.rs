use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::{MacroPluginMetadata, PluginDiagnostic, PluginResult};
use cairo_lang_plugins::plugins::HasItemsInCfgEx;
use cairo_lang_starknet_classes::keccak::starknet_keccak;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{
    GetIdentifier, PathSegmentEx, QueryAttrs, is_single_arg_attr,
};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use const_format::formatcp;
use indoc::formatdoc;
use smol_str::SmolStr;

use super::generation_data::{ContractGenerationData, StarknetModuleCommonGenerationData};
use super::{StarknetModuleKind, grand_grand_parent_starknet_module};
use crate::plugin::consts::{
    ABI_ATTR, ABI_ATTR_EMBED_V0_ARG, ABI_ATTR_PER_ITEM_ARG, COMPONENT_INLINE_MACRO,
    CONCRETE_COMPONENT_STATE_NAME, CONTRACT_STATE_NAME, EVENT_TRAIT, EVENT_TYPE_NAME,
    EXTERNAL_ATTR, HAS_COMPONENT_TRAIT, STORAGE_STRUCT_NAME, SUBSTORAGE_ATTR,
};
use crate::plugin::entry_point::{
    EntryPointGenerationParams, EntryPointKind, EntryPointsGenerationData, handle_entry_point,
};
use crate::plugin::storage::handle_storage_struct;
use crate::plugin::utils::{forbid_attributes_in_impl, has_v0_attribute_ex};

/// Accumulated data specific for contract generation.
#[derive(Default)]
pub struct ContractSpecificGenerationData {
    test_config: RewriteNode,
    entry_points_code: EntryPointsGenerationData,
    components_data: ComponentsGenerationData,
}

/// Data collected by the plugin for eventual validation of the contract.
#[derive(Default)]
struct ComponentsGenerationData {
    /// The components defined in the contract using the `component!` inline macro.
    pub components: Vec<NestedComponent>,
    /// The contract's storage members that are marked with the `substorage` attribute.
    pub substorage_members: Vec<String>,
    /// The contract's event variants that are defined in the main event enum of the contract.
    pub nested_event_variants: Vec<SmolStr>,
}

/// A component defined in a contract using the `component!` inline macro.
pub struct NestedComponent {
    pub component_path: ast::ExprPath,
    pub storage_name: ast::ExprPath,
    pub event_name: ast::ExprPath,
    /// The original ast node of the component usage declaration.
    pub node: ast::ItemInlineMacro,
}

impl ComponentsGenerationData {
    pub fn into_rewrite_node(
        self,
        db: &dyn SyntaxGroup,
        diagnostics: &mut Vec<PluginDiagnostic>,
    ) -> RewriteNode {
        let mut has_component_impls = vec![];
        for NestedComponent { component_path, storage_name, event_name, node } in
            self.components.iter()
        {
            if !self.validate_component(db, diagnostics, storage_name, event_name) {
                // Don't generate the code for the impl of HasComponent.
                continue;
            }
            // TODO(yuval): consider supporting 2 components with the same name and different paths.
            // Currently it doesn't work as the name of the impl is the same.
            let component_name = match component_path.elements(db).last().unwrap() {
                ast::PathSegment::WithGenericArgs(x) => x.ident(db),
                ast::PathSegment::Simple(x) => x.ident(db),
            };

            let has_component_impl = RewriteNode::interpolate_patched(
                &formatdoc!(
                    "impl HasComponentImpl_$component_name$ of \
                     $component_path$::{HAS_COMPONENT_TRAIT}<{CONTRACT_STATE_NAME}> {{
           fn get_component(self: @{CONTRACT_STATE_NAME}) -> \
                     @$component_path$::{CONCRETE_COMPONENT_STATE_NAME} {{
                        @$component_path$::unsafe_new_component_state::<{CONTRACT_STATE_NAME}>()
           }}
           fn get_component_mut(ref self: {CONTRACT_STATE_NAME}) -> \
                     $component_path$::{CONCRETE_COMPONENT_STATE_NAME} {{
               $component_path$::unsafe_new_component_state::<{CONTRACT_STATE_NAME}>()
           }}
           fn get_contract(self: @$component_path$::{CONCRETE_COMPONENT_STATE_NAME}) -> \
                     @{CONTRACT_STATE_NAME} {{
               @unsafe_new_contract_state()
           }}
           fn get_contract_mut(ref self: $component_path$::{CONCRETE_COMPONENT_STATE_NAME}) -> \
                     {CONTRACT_STATE_NAME} {{
               unsafe_new_contract_state()
           }}
           fn emit<S, impl IntoImp: core::traits::Into<S, \
                     $component_path$::{EVENT_TYPE_NAME}>>(ref self: \
                     $component_path$::{CONCRETE_COMPONENT_STATE_NAME}, event: S) {{
               let event: $component_path$::{EVENT_TYPE_NAME} = core::traits::Into::into(event);
               let mut contract = $component_path$::{HAS_COMPONENT_TRAIT}::get_contract_mut(ref \
                     self);
               ContractStateEventEmitter::emit(ref contract, Event::$event_name$(event));
           }}
       }}
       "
                ),
                &[
                    ("component_name".to_string(), RewriteNode::from_ast_trimmed(&component_name)),
                    ("component_path".to_string(), RewriteNode::from_ast_trimmed(component_path)),
                    ("storage_name".to_string(), RewriteNode::from_ast_trimmed(storage_name)),
                    ("event_name".to_string(), RewriteNode::from_ast_trimmed(event_name)),
                ]
                .into(),
            )
            .mapped(db, node);

            has_component_impls.push(has_component_impl);
        }
        RewriteNode::new_modified(has_component_impls)
    }

    /// Validate the component
    fn validate_component(
        &self,
        db: &dyn SyntaxGroup,
        diagnostics: &mut Vec<PluginDiagnostic>,
        storage_name: &ast::ExprPath,
        event_name: &ast::ExprPath,
    ) -> bool {
        let mut is_valid = true;

        let storage_name_syntax_node = storage_name.as_syntax_node();
        if !self.substorage_members.contains(&storage_name_syntax_node.get_text(db)) {
            diagnostics.push(PluginDiagnostic::error(
                storage_name.stable_ptr().untyped(),
                format!(
                    "`{0}` is not a substorage member in the contract's \
                     `{STORAGE_STRUCT_NAME}`.\nConsider adding to \
                     `{STORAGE_STRUCT_NAME}`:\n```\n#[{SUBSTORAGE_ATTR}(v0)]\n{0}: \
                     path::to::component::{STORAGE_STRUCT_NAME},\n````",
                    storage_name_syntax_node.get_text_without_trivia(db)
                )
                .to_string(),
            ));
            is_valid = false;
        }

        let event_name_str = event_name.as_syntax_node().get_text_without_trivia(db);
        if !self.nested_event_variants.contains(&event_name_str.clone().into()) {
            diagnostics.push(PluginDiagnostic::error(
                event_name.stable_ptr().untyped(),
                format!(
                    "`{event_name_str}` is not a nested event in the contract's \
                     `{EVENT_TYPE_NAME}` enum.\nConsider adding to the `{EVENT_TYPE_NAME}` \
                     enum:\n```\n{event_name_str}: \
                     path::to::component::{EVENT_TYPE_NAME},\n```\nNote: currently with \
                     components, only an enum {EVENT_TYPE_NAME} directly in the contract is \
                     supported.",
                )
                .to_string(),
            ));
            is_valid = false;
        }

        is_valid
    }
}

/// The event emitter code for a contract.
const EVENT_EMITTER_CODE: &str = formatcp! {
"    impl {CONTRACT_STATE_NAME}EventEmitter of starknet::event::EventEmitter<
        {CONTRACT_STATE_NAME}, {EVENT_TYPE_NAME}
    > {{
        fn emit<S, impl IntoImp: core::traits::Into<S, {EVENT_TYPE_NAME}>>(
            ref self: {CONTRACT_STATE_NAME}, event: S
        ) {{
            let event: {EVENT_TYPE_NAME} = core::traits::Into::into(event);
            let mut keys = Default::<core::array::Array>::default();
            let mut data = Default::<core::array::Array>::default();
            {EVENT_TRAIT}::append_keys_and_data(@event, ref keys, ref data);
            starknet::SyscallResultTrait::unwrap_syscall(
                starknet::syscalls::emit_event_syscall(
                    core::array::ArrayTrait::span(@keys),
                    core::array::ArrayTrait::span(@data),
                )
            )
        }}
    }}
"
};

impl ContractSpecificGenerationData {
    pub fn into_rewrite_node(
        self,
        db: &dyn SyntaxGroup,
        diagnostics: &mut Vec<PluginDiagnostic>,
    ) -> RewriteNode {
        RewriteNode::interpolate_patched(
            &formatdoc! {"
                // TODO(Gil): This generates duplicate diagnostics because of the plugin system, squash the duplicates into one.
                #[deprecated(
                    feature: \"deprecated_legacy_map\",
                    note: \"Use `starknet::storage::Map` instead.\"
                )]
                #[allow(unused_imports)]
                use starknet::storage::Map as LegacyMap;
                $test_config$
                $entry_points_code$
                {EVENT_EMITTER_CODE}
                $components_code$"},
            &[
                ("test_config".to_string(), self.test_config),
                ("entry_points_code".to_string(), self.entry_points_code.into_rewrite_node()),
                (
                    "components_code".to_string(),
                    self.components_data.into_rewrite_node(db, diagnostics),
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
    item: &ast::ModuleItem,
    metadata: &MacroPluginMetadata<'_>,
    data: &mut ContractGenerationData,
) {
    match item {
        ast::ModuleItem::FreeFunction(item_function) => {
            handle_contract_free_function(
                db,
                diagnostics,
                item_function,
                &mut data.specific.entry_points_code,
            );
        }
        ast::ModuleItem::Impl(item_impl) => {
            handle_contract_impl(
                db,
                diagnostics,
                item_impl,
                metadata,
                &mut data.specific.entry_points_code,
            );
        }
        ast::ModuleItem::Struct(item_struct)
            if item_struct.name(db).text(db) == STORAGE_STRUCT_NAME =>
        {
            handle_storage_struct(
                db,
                diagnostics,
                item_struct.clone(),
                StarknetModuleKind::Contract,
                &mut data.common,
                metadata,
            );
            for member in item_struct.members(db).elements(db) {
                // v0 is not validated here to not create multiple diagnostics. It's already
                // verified in handle_storage_struct above.
                if member.has_attr(db, SUBSTORAGE_ATTR) {
                    data.specific
                        .components_data
                        .substorage_members
                        .push(member.name(db).text(db).to_string());
                }
            }
        }
        ast::ModuleItem::ImplAlias(alias_ast) => {
            let abi_attrs = alias_ast.query_attr(db, ABI_ATTR);
            if abi_attrs.is_empty() {
                return;
            }
            if abi_attrs.iter().all(|attr| is_single_arg_attr(db, attr, ABI_ATTR_EMBED_V0_ARG)) {
                handle_embed_impl_alias(
                    db,
                    diagnostics,
                    alias_ast,
                    &mut data.specific.entry_points_code,
                );
            } else {
                diagnostics.push(PluginDiagnostic::error(
                    alias_ast.stable_ptr().untyped(),
                    format!(
                        "The '{ABI_ATTR}' attribute for impl aliases only supports the \
                         '{ABI_ATTR_EMBED_V0_ARG}' argument.",
                    ),
                ));
            }
        }
        ast::ModuleItem::InlineMacro(inline_macro_ast)
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
    metadata: &MacroPluginMetadata<'_>,
    event_variants: Vec<SmolStr>,
) -> RewriteNode {
    let mut generation_data = ContractGenerationData { common: common_data, ..Default::default() };
    generation_data.specific.components_data.nested_event_variants = event_variants;
    for item in body.iter_items_in_cfg(db, metadata.cfg_set) {
        handle_contract_item(db, diagnostics, &item, metadata, &mut generation_data);
    }

    let test_class_hash = format!(
        "0x{:x}",
        starknet_keccak(module_ast.as_syntax_node().get_text_without_trivia(db).as_bytes(),)
    );

    generation_data.specific.test_config = RewriteNode::Text(formatdoc!(
        "#[cfg(target: 'test')]
            pub const TEST_CLASS_HASH: starknet::ClassHash = {test_class_hash}.try_into().unwrap();
"
    ));

    generation_data.into_rewrite_node(db, diagnostics)
}

/// Handles a contract entrypoint function.
fn handle_contract_entry_point(
    entry_point_kind: EntryPointKind,
    item_function: &ast::FunctionWithBody,
    wrapped_function_path: RewriteNode,
    wrapper_identifier: String,
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
            wrapper_identifier,
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
    let function_name = item_function.declaration(db).name(db);
    let function_name_node = RewriteNode::from_ast_trimmed(&function_name);
    handle_contract_entry_point(
        entry_point_kind,
        item_function,
        function_name_node,
        function_name.text(db).into(),
        db,
        diagnostics,
        data,
    );
}

/// Handles an impl inside a contract module.
fn handle_contract_impl(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    imp: &ast::ItemImpl,
    metadata: &MacroPluginMetadata<'_>,
    data: &mut EntryPointsGenerationData,
) {
    let abi_config = impl_abi_config(db, diagnostics, imp);
    if abi_config == ImplAbiConfig::None {
        return;
    }
    let ast::MaybeImplBody::Some(impl_body) = imp.body(db) else {
        return;
    };
    let impl_name = imp.name(db);
    let impl_name_node = RewriteNode::from_ast_trimmed(&impl_name);
    for item in impl_body.iter_items_in_cfg(db, metadata.cfg_set) {
        if abi_config == ImplAbiConfig::Embed {
            forbid_attributes_in_impl(db, diagnostics, &item, "#[abi(embed_v0)]");
        } else if abi_config == ImplAbiConfig::External {
            forbid_attributes_in_impl(db, diagnostics, &item, "#[external(v0)]");
        }

        let ast::ImplItem::Function(item_function) = item else {
            continue;
        };
        let entry_point_kind = if abi_config == ImplAbiConfig::PerItem {
            let Some(entry_point_kind) =
                EntryPointKind::try_from_attrs(db, diagnostics, &item_function)
            else {
                continue;
            };
            entry_point_kind
        } else {
            // matches!(abi_config, ImplAbiConfig::Embed | ImplAbiConfig::External)
            EntryPointKind::External
        };
        let function_name = item_function.declaration(db).name(db);
        let function_name_node = RewriteNode::interpolate_patched(
            "$impl_name$::$func_name$",
            &[
                ("impl_name".to_string(), impl_name_node.clone()),
                ("func_name".to_string(), RewriteNode::from_ast_trimmed(&function_name)),
            ]
            .into(),
        );
        let wrapper_identifier = format!("{}__{}", impl_name.text(db), function_name.text(db));
        handle_contract_entry_point(
            entry_point_kind,
            &item_function,
            function_name_node,
            wrapper_identifier,
            db,
            diagnostics,
            data,
        );
    }
}

/// The configuration of an impl addition to the abi.
#[derive(PartialEq, Eq)]
enum ImplAbiConfig {
    /// No ABI configuration.
    None,
    /// The impl is marked with `#[abi(per_item)]`. Each item should provide its own configuration.
    PerItem,
    /// The impl is marked with `#[abi(embed_v0)]`. The entire impl and the interface are embedded
    /// into the ABI.
    Embed,
    /// The impl is marked with `#[external(v0)]`. The entire impl is embedded into the ABI as
    /// external functions.
    External,
}

/// Returns the configuration of an impl addition to the abi using `#[abi(...)]` or the old
/// equivalent `#[external(v0)]`.
fn impl_abi_config(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    imp: &ast::ItemImpl,
) -> ImplAbiConfig {
    if let Some(abi_attr) = imp.find_attr(db, ABI_ATTR) {
        if is_single_arg_attr(db, &abi_attr, ABI_ATTR_PER_ITEM_ARG) {
            ImplAbiConfig::PerItem
        } else if is_single_arg_attr(db, &abi_attr, ABI_ATTR_EMBED_V0_ARG) {
            ImplAbiConfig::Embed
        } else {
            diagnostics.push(PluginDiagnostic::error(
                abi_attr.stable_ptr().untyped(),
                format!(
                    "The '{ABI_ATTR}' attribute for impls only supports the \
                     '{ABI_ATTR_PER_ITEM_ARG}' or '{ABI_ATTR_EMBED_V0_ARG}' argument.",
                ),
            ));
            ImplAbiConfig::None
        }
    } else if has_v0_attribute_ex(db, diagnostics, imp, EXTERNAL_ATTR, || {
        Some(format!(
            "The '{EXTERNAL_ATTR}' attribute on impls is deprecated. Use \
             '{ABI_ATTR}({ABI_ATTR_PER_ITEM_ARG})' or '{ABI_ATTR}({ABI_ATTR_EMBED_V0_ARG})'."
        ))
    }) {
        ImplAbiConfig::External
    } else {
        ImplAbiConfig::None
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
        diagnostics.push(PluginDiagnostic::error(
            alias_ast.stable_ptr().untyped(),
            format!(
                "Generic parameters are not supported in impl aliases with \
                 `#[{ABI_ATTR}({ABI_ATTR_EMBED_V0_ARG})]`."
            ),
        ));
        return;
    }
    let elements = alias_ast.impl_path(db).elements(db);
    let Some((impl_final_part, impl_module)) = elements.split_last() else {
        unreachable!("impl_path should have at least one segment")
    };

    if !is_first_generic_arg_contract_state(db, impl_final_part) {
        diagnostics.push(PluginDiagnostic::error(
            alias_ast.stable_ptr().untyped(),
            format!(
                "First generic argument of impl alias with \
                 `#[{ABI_ATTR}({ABI_ATTR_EMBED_V0_ARG})]` must be `{CONTRACT_STATE_NAME}`."
            ),
        ));
        return;
    }
    let impl_name = impl_final_part.identifier_ast(db);
    let impl_module = RewriteNode::interspersed(
        impl_module.iter().map(RewriteNode::from_ast_trimmed),
        RewriteNode::text("::"),
    );
    data.generated_wrapper_functions.push(
        RewriteNode::interpolate_patched(
            &formatdoc! {"
            impl ContractState$impl_name$ of
                $impl_module$::UnsafeNewContractStateTraitFor$impl_name$<{CONTRACT_STATE_NAME}> {{
                fn unsafe_new_contract_state() -> {CONTRACT_STATE_NAME} {{
                    unsafe_new_contract_state()
                }}
            }}
        "},
            &[
                ("impl_name".to_string(), RewriteNode::from_ast_trimmed(&impl_name)),
                ("impl_module".to_string(), impl_module),
            ]
            .into(),
        )
        .mapped(db, alias_ast),
    );
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
        ast::WrappedArgList::ParenthesizedArgList(args) => args.arguments(db),
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

    let (Some(component_path), Some(storage_name), Some(event_name)) = (
        try_extract_named_macro_argument(db, diagnostics, path_arg, "path", false),
        try_extract_named_macro_argument(db, diagnostics, storage_arg, "storage", true),
        try_extract_named_macro_argument(db, diagnostics, event_arg, "event", true),
    ) else {
        return;
    };

    data.components_data.components.push(NestedComponent {
        component_path: component_path.clone(),
        storage_name: storage_name.clone(),
        event_name: event_name.clone(),
        node: component_macro_ast.clone(),
    });
}

/// Returns an invalid `component` macro diagnostic.
fn invalid_macro_diagnostic(component_macro_ast: &ast::ItemInlineMacro) -> PluginDiagnostic {
    PluginDiagnostic::error(
        component_macro_ast.stable_ptr().untyped(),
        format!(
            "Invalid component macro, expected `{COMPONENT_INLINE_MACRO}!(name: \
             \"<component_name>\", storage: \"<storage_name>\", event: \"<event_name>\");`"
        ),
    )
}

/// Remove a `component!` inline macro from the original code if it's inside a starknet::contract.
/// Assumes that the macro name is `COMPONENT_INLINE_MACRO`.
pub fn remove_component_inline_macro(
    db: &dyn SyntaxGroup,
    component_macro_ast: &ast::ItemInlineMacro,
) -> PluginResult {
    if let Some((_module_ast, module_kind, _)) =
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
                        diagnostics.push(PluginDiagnostic::error(
                            path.stable_ptr().untyped(),
                            format!(
                                "Component macro argument `{arg_name}` must be a simple \
                                 identifier.",
                            ),
                        ));
                        return None;
                    }
                    Some(path)
                }
                value => {
                    diagnostics.push(PluginDiagnostic::error(
                        value.stable_ptr().untyped(),
                        format!(
                            "Component macro argument `{arg_name}` must be a path expression.",
                        ),
                    ));
                    None
                }
            }
        }
        _ => {
            diagnostics.push(PluginDiagnostic::error(
                arg_ast.stable_ptr().untyped(),
                format!("Invalid component macro argument. Expected `{0}: <value>`", arg_name),
            ));
            None
        }
    }
}
