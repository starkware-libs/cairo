use std::vec;

use cairo_lang_defs::db::get_all_path_leafs;
use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::node::ast::MaybeModuleBody;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, QueryAttrs};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use indoc::formatdoc;

use super::consts::{
    COMPONENT_ATTR, CONSTRUCTOR_ATTR, CONSTRUCTOR_MODULE, CONTRACT_ATTR, CONTRACT_STATE_NAME,
    DEPRECATED_CONTRACT_ATTR, EVENT_ATTR, EVENT_TYPE_NAME, EXTERNAL_ATTR, EXTERNAL_MODULE,
    INCLUDE_ATTR, L1_HANDLER_ATTR, L1_HANDLER_MODULE, STORAGE_ATTR, STORAGE_STRUCT_NAME,
};
use super::entry_point::{
    handle_entry_point, has_external_attribute, has_include_attribute, EntryPointGenerationParams,
    EntryPointKind, EntryPointsGenerationData,
};
use super::storage::handle_storage_struct;
use crate::contract::starknet_keccak;
use crate::plugin::aux_data::StarkNetContractAuxData;

/// Handles a contract/component module item.
pub fn handle_module(db: &dyn SyntaxGroup, module_ast: ast::ItemModule) -> PluginResult {
    if module_ast.has_attr(db, DEPRECATED_CONTRACT_ATTR) {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: format!(
                    "The '{DEPRECATED_CONTRACT_ATTR}' attribute was deprecated, please use \
                     `{CONTRACT_ATTR}` instead.",
                ),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    }
    if let Some(kind) = StarknetModuleKind::from_module(db, &module_ast) {
        return validate_module(db, module_ast, kind.to_str_capital());
    }

    PluginResult::default()
}

/// Validates the contract/component module (has body with storage named 'Storage').
fn validate_module(
    db: &dyn SyntaxGroup,
    module_ast: ast::ItemModule,
    module_kind_str: &str,
) -> PluginResult {
    let MaybeModuleBody::Some(body) = module_ast.body(db) else {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: format!("{module_kind_str}s without body are not supported."),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    };
    let Some(storage_struct_ast) = body.items(db).elements(db).into_iter().find(|item| {
        matches!(item, ast::Item::Struct(struct_ast) if struct_ast.name(db).text(db) == STORAGE_STRUCT_NAME)
    }) else {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: format!("{module_kind_str}s must define a 'Storage' struct."),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    };

    if !storage_struct_ast.has_attr(db, STORAGE_ATTR) {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: "'Storage' struct must be annotated with #[storage].".to_string(),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    }

    PluginResult::default()
}

/// The kind of the starknet module (contract/component).
#[derive(PartialEq, Eq, Copy, Clone)]
pub enum StarknetModuleKind {
    Contract,
    Component,
}
impl StarknetModuleKind {
    /// Returns the starknet module kind according to the module's attributes, if any.
    fn from_module(db: &dyn SyntaxGroup, module_ast: &ast::ItemModule) -> Option<Self> {
        if module_ast.has_attr(db, CONTRACT_ATTR) {
            Some(StarknetModuleKind::Contract)
        } else if module_ast.has_attr(db, COMPONENT_ATTR) {
            Some(StarknetModuleKind::Component)
        } else {
            None
        }
    }
    /// Returns the name of the kind, with a leading capital letter.
    pub fn to_str_capital(self) -> &'static str {
        match self {
            Self::Contract => "Contract",
            Self::Component => "Component",
        }
    }
    /// Returns the name of the kind, lower case.
    pub fn to_str_lower(self) -> &'static str {
        match self {
            Self::Contract => "contract",
            Self::Component => "component",
        }
    }
}

/// If the module is annotated with CONTRACT_ATTR or COMPONENT_ATTR, generate the relevant
/// contract/component logic.
pub fn handle_module_by_storage(
    db: &dyn SyntaxGroup,
    struct_ast: ast::ItemStruct,
) -> Option<PluginResult> {
    let module_node = struct_ast.as_syntax_node().parent()?.parent()?.parent()?;
    if module_node.kind(db) != SyntaxKind::ItemModule {
        return None;
    }
    let module_ast = ast::ItemModule::from_syntax_node(db, module_node);
    let module_kind = StarknetModuleKind::from_module(db, &module_ast)?;

    let body = extract_matches!(module_ast.body(db), MaybeModuleBody::Some);
    let mut diagnostics = vec![];

    // Use declarations to add to the internal submodules. Mapping from 'use' items to their path.
    let mut extra_uses = OrderedHashMap::default();
    // Whether an event exists in the given module. If it doesn't, we need to generate an empty one.
    let mut has_event = false;
    for item in body.items(db).elements(db) {
        // Skip elements that only generate other code, but their code itself is ignored.
        if matches!(&item, ast::Item::Struct(item) if item.name(db).text(db) == STORAGE_STRUCT_NAME)
        {
            continue;
        }

        if is_starknet_event(db, &mut diagnostics, &item, module_kind) {
            has_event = true;
        }

        maybe_add_extra_use(db, item, &mut extra_uses);
    }

    let extra_uses_node = RewriteNode::new_modified(
        extra_uses
            .values()
            .map(|use_path| RewriteNode::Text(format!("\n        use {use_path};")))
            .collect(),
    );

    let mut data = EntryPointsGenerationData::default();

    // Generate the code for ContractState/ComponentState and the entry points.
    let state_struct_code = generate_entry_points_and_state_struct(
        db,
        &mut diagnostics,
        body,
        &mut data,
        &extra_uses_node,
        has_event,
        module_kind,
    );

    let module_name_ast = module_ast.name(db);
    let test_class_hash = format!(
        "0x{:x}",
        starknet_keccak(
            module_ast.as_syntax_node().get_text_without_trivia(db).as_str().as_bytes(),
        )
    );

    let internal_modules_node = match module_kind {
        StarknetModuleKind::Contract => {
            let generated_external_module = generate_submodule(
                EXTERNAL_MODULE,
                RewriteNode::new_modified(data.external_functions),
            );
            let generated_l1_handler_module = generate_submodule(
                L1_HANDLER_MODULE,
                RewriteNode::new_modified(data.l1_handler_functions),
            );
            let generated_constructor_module = generate_submodule(
                CONSTRUCTOR_MODULE,
                RewriteNode::new_modified(data.constructor_functions),
            );
            RewriteNode::interpolate_patched(
                "    $generated_external_module$
    $generated_l1_handler_module$
    $generated_constructor_module$",
                [
                    ("generated_external_module".to_string(), generated_external_module),
                    ("generated_l1_handler_module".to_string(), generated_l1_handler_module),
                    ("generated_constructor_module".to_string(), generated_constructor_module),
                ]
                .into(),
            )
        }
        StarknetModuleKind::Component => RewriteNode::empty(),
    };
    let test_config = match module_kind {
        StarknetModuleKind::Contract => formatdoc!(
            "
            #[cfg(test)]
            const TEST_CLASS_HASH: felt252 = {test_class_hash};"
        ),
        StarknetModuleKind::Component => "".to_string(),
    };
    let generated_module = RewriteNode::interpolate_patched(
        formatdoc!(
            "
            {test_config}
            $state_struct_code$
            $generated_wrapper_functions$
            $internal_modules_node$
        "
        )
        .as_str(),
        [
            ("state_struct_code".to_string(), state_struct_code),
            (
                "generated_wrapper_functions".to_string(),
                RewriteNode::new_modified(data.generated_wrapper_functions),
            ),
            ("internal_modules_node".to_string(), internal_modules_node),
        ]
        .into(),
    );

    let mut builder = PatchBuilder::new(db);
    builder.add_modified(generated_module);
    Some(PluginResult {
        code: Some(PluginGeneratedFile {
            name: module_kind.to_str_lower().into(),
            content: builder.code,
            patches: builder.patches,
            aux_data: match module_kind {
                StarknetModuleKind::Contract => {
                    Some(DynGeneratedFileAuxData::new(StarkNetContractAuxData {
                        contracts: vec![module_name_ast.text(db)],
                    }))
                }
                StarknetModuleKind::Component => None,
            },
        }),
        diagnostics,
        remove_original_item: true,
    })
}

/// Generates a submodule with the given name, uses and functions.
fn generate_submodule(module_name: &str, generated_functions_node: RewriteNode) -> RewriteNode {
    let generated_external_module = RewriteNode::interpolate_patched(
        formatdoc!(
            "
            mod {module_name} {{$generated_functions_node$
                }}
        "
        )
        .as_str(),
        [("generated_functions_node".to_string(), generated_functions_node)].into(),
    );
    generated_external_module
}

/// Generates the code for the entry points of the contract/component, and for its state struct.
fn generate_entry_points_and_state_struct(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    body: ast::ModuleBody,
    data: &mut EntryPointsGenerationData,
    extra_uses_node: &RewriteNode,
    has_event: bool,
    module_kind: StarknetModuleKind,
) -> RewriteNode {
    let mut state_struct_code = RewriteNode::Text("".to_string());
    for item in body.items(db).elements(db) {
        match &item {
            ast::Item::FreeFunction(item_function)
                if module_kind == StarknetModuleKind::Contract =>
            {
                handle_contract_free_function(db, diagnostics, item_function, data);
            }
            ast::Item::Impl(item_impl) if module_kind == StarknetModuleKind::Contract => {
                handle_contract_impl(db, diagnostics, &item, item_impl, data);
            }
            ast::Item::Struct(item_struct)
                if item_struct.name(db).text(db) == STORAGE_STRUCT_NAME =>
            {
                let (state_struct_rewrite_node, storage_diagnostics) = handle_storage_struct(
                    db,
                    item_struct.clone(),
                    extra_uses_node,
                    has_event,
                    module_kind,
                );
                state_struct_code = state_struct_rewrite_node;
                diagnostics.extend(storage_diagnostics);
            }
            ast::Item::ImplAlias(alias_ast) if alias_ast.has_attr(db, INCLUDE_ATTR) => {
                let no_generic_params = match alias_ast.generic_params(db) {
                    ast::OptionWrappedGenericParamList::Empty(_) => true,
                    ast::OptionWrappedGenericParamList::WrappedGenericParamList(generics) => {
                        generics.generic_params(db).elements(db).is_empty()
                    }
                };
                if !no_generic_params {
                    diagnostics.push(PluginDiagnostic {
                        stable_ptr: alias_ast.stable_ptr().untyped(),
                        message: "Generic parameters are not supported in include impl aliases."
                            .to_string(),
                    });
                    continue;
                }
                let elements = alias_ast.impl_path(db).elements(db);
                let Some((impl_final_part, impl_module)) = elements.split_last() else {
                    unreachable!("impl_path should have at least one segment")
                };
                let (impl_name, first_arg_is_contract_storage) = match impl_final_part {
                    ast::PathSegment::WithGenericArgs(segment) => (
                        segment.ident(db),
                        match segment.generic_args(db).generic_args(db).elements(db).first() {
                            Some(ast::GenericArg::Expr(expr)) => match expr.value(db) {
                                ast::Expr::Path(path) => path.identifier(db) == CONTRACT_STATE_NAME,
                                _ => false,
                            },
                            _ => false,
                        },
                    ),
                    ast::PathSegment::Simple(segment) => (segment.ident(db), false),
                };
                if !first_arg_is_contract_storage {
                    diagnostics.push(PluginDiagnostic {
                        stable_ptr: alias_ast.stable_ptr().untyped(),
                        message: "First argument of include impl alias should be `ContractState`."
                            .to_string(),
                    });
                    continue;
                }
                let impl_module = RewriteNode::new_modified(
                    impl_module
                        .iter()
                        .flat_map(|e| {
                            vec![
                                RewriteNode::new_trimmed(e.as_syntax_node()),
                                RewriteNode::Text("::".to_string()),
                            ]
                        })
                        .collect(),
                );
                data.generated_wrapper_functions.push(RewriteNode::interpolate_patched(formatdoc! {"
                    impl ContractState$impl_name$ of $impl_module$UnsafeNewContractStateTraitFor$impl_name$<{CONTRACT_STATE_NAME}> {{
                        fn unsafe_new_contract_state() -> {CONTRACT_STATE_NAME} {{
                            unsafe_new_contract_state()
                        }}
                    }}
                "}.as_str(),
                [
                    ("impl_name".to_string(), RewriteNode::new_trimmed(impl_name.as_syntax_node())),
                    ("impl_module".to_string(), impl_module),
                ].into()))
            }
            _ => {}
        }
    }
    state_struct_code
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
    if !(is_external || has_include_attribute(db, diagnostics, item)) {
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
        let entry_point_kind = if is_external || item_function.has_attr(db, EXTERNAL_ATTR) {
            EntryPointKind::External
        } else if item_function.has_attr(db, CONSTRUCTOR_ATTR) {
            EntryPointKind::Constructor
        } else if item_function.has_attr(db, L1_HANDLER_ATTR) {
            EntryPointKind::L1Handler
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

/// Adds extra uses, to be used in the generated submodules.
fn maybe_add_extra_use(
    db: &dyn SyntaxGroup,
    item: ast::Item,
    extra_uses: &mut OrderedHashMap<smol_str::SmolStr, String>,
) {
    if let Some(ident) = match item {
        ast::Item::Use(item) => {
            let leaves = get_all_path_leafs(db, item.use_path(db));
            for leaf in leaves {
                extra_uses
                    .entry(leaf.stable_ptr().identifier(db))
                    .or_insert_with_key(|ident| format!("super::{}", ident));
            }
            None
        }
        ast::Item::Constant(item) => Some(item.name(db)),
        ast::Item::Module(item) => Some(item.name(db)),
        ast::Item::Impl(item) => Some(item.name(db)),
        ast::Item::Struct(item) => Some(item.name(db)),
        ast::Item::Enum(item) => Some(item.name(db)),
        ast::Item::TypeAlias(item) => Some(item.name(db)),
        // These items are not directly required in generated inner modules.
        ast::Item::ExternFunction(_)
        | ast::Item::ExternType(_)
        | ast::Item::Trait(_)
        | ast::Item::FreeFunction(_)
        | ast::Item::ImplAlias(_)
        | ast::Item::Missing(_) => None,
    } {
        extra_uses.entry(ident.text(db)).or_insert_with_key(|ident| format!("super::{}", ident));
    }
}

/// Checks whether the given item is an event, and if so - makes sure it's valid.
fn is_starknet_event(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
    module_kind: StarknetModuleKind,
) -> bool {
    let (has_event_name, stable_ptr) = match item {
        ast::Item::Struct(strct) => {
            (strct.name(db).text(db) == EVENT_TYPE_NAME, strct.name(db).stable_ptr().untyped())
        }
        ast::Item::Enum(enm) => {
            (enm.name(db).text(db) == EVENT_TYPE_NAME, enm.name(db).stable_ptr().untyped())
        }
        ast::Item::Use(item) => {
            for leaf in get_all_path_leafs(db, item.use_path(db)) {
                let stable_ptr = &leaf.stable_ptr();
                if stable_ptr.identifier(db) == EVENT_TYPE_NAME {
                    if !item.has_attr(db, EVENT_ATTR) {
                        diagnostics.push(PluginDiagnostic {
                            message: format!(
                                "{} type that is named `Event` must be marked with \
                                 #[{EVENT_ATTR}].",
                                module_kind.to_str_capital()
                            ),
                            stable_ptr: stable_ptr.untyped(),
                        });
                    }
                    return true;
                }
            }
            return false;
        }
        _ => return false,
    };
    let has_event_attr = item.has_attr(db, EVENT_ATTR);

    match (has_event_attr, has_event_name) {
        (true, false) => {
            diagnostics.push(PluginDiagnostic {
                message: format!(
                    "{} type that is marked with #[{EVENT_ATTR}] must be named `Event`.",
                    module_kind.to_str_capital()
                ),
                stable_ptr,
            });
            false
        }
        (false, true) => {
            diagnostics.push(PluginDiagnostic {
                message: format!(
                    "{} type that is named `Event` must be marked with #[{EVENT_ATTR}].",
                    module_kind.to_str_capital()
                ),
                stable_ptr,
            });
            // The attribute is missing, but this counts as a event - we can't create another
            // (empty) event.
            true
        }
        (true, true) => true,
        (false, false) => false,
    }
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
            unsafe_new_contract_state: RewriteNode::Text("unsafe_new_contract_state()".to_string()),
            generic_params: RewriteNode::empty(),
        },
        diagnostics,
        data,
    )
}
