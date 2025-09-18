use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::{MacroPluginMetadata, PluginDiagnostic};
use cairo_lang_syntax::node::helpers::{GetIdentifier, QueryAttrs};
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use indoc::formatdoc;
use itertools::zip_eq;
use salsa::Database;

use super::starknet_module::generation_data::StarknetModuleCommonGenerationData;
use super::starknet_module::{StarknetModuleKind, backwards_compatible_storage};
use super::storage_interfaces::{
    StorageMemberConfig, StorageMemberKind, handle_storage_interface_struct,
    struct_members_storage_configs,
};
use super::{CONCRETE_COMPONENT_STATE_NAME, CONTRACT_STATE_NAME, FLAT_ATTR, STORAGE_STRUCT_NAME};
use crate::plugin::SUBSTORAGE_ATTR;

/// Generate getters and setters for the members of the storage struct.
pub fn handle_storage_struct<'db, 'a>(
    db: &'db dyn Database,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    struct_ast: ast::ItemStruct<'db>,
    starknet_module_kind: StarknetModuleKind,
    data: &mut StarknetModuleCommonGenerationData<'db>,
    metadata: &'a MacroPluginMetadata<'a>,
) {
    let state_struct_name = starknet_module_kind.get_state_struct_name();
    let generic_arg_str = starknet_module_kind.get_generic_arg_str();
    let full_generic_arg_str = starknet_module_kind.get_full_generic_arg_str();
    let full_state_struct_name = starknet_module_kind.get_full_state_struct_name();

    let mut members_struct_code = vec![];
    let mut members_struct_code_mut = vec![];
    let mut members_init_code = vec![];
    let mut substorage_members_struct_code = vec![];
    let mut substorage_members_init_code = vec![];
    let mut storage_struct_members = vec![];
    let configs = struct_members_storage_configs(db, &struct_ast, diagnostics);
    for (member, config) in zip_eq(struct_ast.members(db).elements(db), &configs) {
        if config.kind == StorageMemberKind::SubStorage {
            if let Some((struct_code, init_code)) =
                get_substorage_member_code(db, &member, metadata)
            {
                substorage_members_struct_code.push(struct_code);
                substorage_members_init_code.push(init_code);
            } else {
                diagnostics.push(PluginDiagnostic::error(
                    member.stable_ptr(db),
                    format!(
                        "`{SUBSTORAGE_ATTR}` attribute is only allowed for members of type \
                         [some_path::]{STORAGE_STRUCT_NAME}`"
                    ),
                ));
            }
        }
        let SimpleMemberGeneratedCode { struct_code, struct_code_mut, init_code, storage_member } =
            get_simple_member_code(db, &member, config, metadata);
        members_struct_code.push(struct_code);
        members_struct_code_mut.push(struct_code_mut);
        members_init_code.push(init_code);
        storage_struct_members.push(storage_member);
    }

    let module_kind = starknet_module_kind.to_str_lower();
    let unsafe_new_function_name = format!("unsafe_new_{module_kind}_state");
    let storage_struct_code = if backwards_compatible_storage(metadata.edition) {
        formatdoc!(
            "
            #[phantom]
            pub struct Storage {{$storage_struct_members$
            }}
            "
        )
    } else {
        "".to_string()
    };
    let storage_base_code =
        handle_storage_interface_struct(db, &struct_ast, &configs, metadata).into_rewrite_node();
    data.state_struct_code = RewriteNode::interpolate_patched(
        &formatdoc!(
            "
            {storage_struct_code}
            $storage_base_code$
            pub struct {full_state_struct_name} {{$substorage_members_struct_code$
            }}

            impl {state_struct_name}Drop{generic_arg_str} of Drop<{full_state_struct_name}> {{}}

            impl {state_struct_name}Deref{generic_arg_str} of \
             core::ops::Deref<@{full_state_struct_name}> {{
                type Target = starknet::storage::FlattenedStorage<Storage>;
                fn deref(self: @{full_state_struct_name}) -> \
             starknet::storage::FlattenedStorage<Storage> {{
                    starknet::storage::FlattenedStorage {{}}
                }}
            }}
            impl {state_struct_name}DerefMut{generic_arg_str} of \
             core::ops::DerefMut<{full_state_struct_name}> {{
                type Target = \
             starknet::storage::FlattenedStorage<starknet::storage::Mutable<Storage>> ;
                fn deref_mut(ref self: {full_state_struct_name}) -> \
             starknet::storage::FlattenedStorage<starknet::storage::Mutable<Storage>> {{
                    starknet::storage::FlattenedStorage {{}}
                }}
            }}
            pub fn {unsafe_new_function_name}{generic_arg_str}() -> {full_state_struct_name} {{
                {state_struct_name}{full_generic_arg_str} {{$substorage_members_init_code$
                }}
            }}
            #[cfg(target: 'test')]
            #[inline(always)]
            pub fn {module_kind}_state_for_testing{generic_arg_str}() -> {full_state_struct_name} \
             {{
                {unsafe_new_function_name}{full_generic_arg_str}()
            }}
            ",
        ),
        &[
            ("storage_base_code".to_string(), storage_base_code),
            (
                "storage_struct_members".to_string(),
                RewriteNode::new_modified(storage_struct_members),
            ),
            (
                "substorage_members_struct_code".to_string(),
                RewriteNode::new_modified(substorage_members_struct_code),
            ),
            (
                "substorage_members_init_code".to_string(),
                RewriteNode::new_modified(substorage_members_init_code),
            ),
            ("members_struct_code".to_string(), RewriteNode::new_modified(members_struct_code)),
            (
                "members_struct_code_mut".to_string(),
                RewriteNode::new_modified(members_struct_code_mut),
            ),
            ("members_init_code".to_string(), RewriteNode::new_modified(members_init_code)),
        ]
        .into(),
    )
    .mapped(db, &struct_ast);
}

/// Returns the relevant code for a substorage storage member.
fn get_substorage_member_code<'db>(
    db: &'db dyn Database,
    member: &ast::Member<'db>,
    metadata: &MacroPluginMetadata<'_>,
) -> Option<(RewriteNode<'db>, RewriteNode<'db>)> {
    let member_visibility = if backwards_compatible_storage(metadata.edition) {
        RewriteNode::text("pub")
    } else {
        RewriteNode::from_ast(&member.visibility(db))
    };
    match member.type_clause(db).ty(db) {
        ast::Expr::Path(type_path) => {
            let segments = type_path.segments(db);
            let mut elements = segments.elements(db);
            // The path has at least one element.
            let last = elements.next_back().unwrap();
            match last {
                ast::PathSegment::Simple(segment)
                    if segment.identifier(db).long(db) == STORAGE_STRUCT_NAME =>
                {
                    let component_path = RewriteNode::interspersed(
                        elements.map(|e| RewriteNode::from_ast_trimmed(&e)),
                        RewriteNode::text("::"),
                    );

                    Some((
                        RewriteNode::interpolate_patched(
                            &format!("\n$attributes$        $member_visibility$ $name$: $component_path$::{CONCRETE_COMPONENT_STATE_NAME},"),
                            &[
                                ("attributes".to_string(), RewriteNode::from_ast(&member.attributes(db))),
                                (
                                    "member_visibility".to_string(),
                                    member_visibility,
                                ),
                                (
                                    "name".to_string(),
                                    RewriteNode::from_ast_trimmed(&member.name(db)),
                                ),
                                ("component_path".to_string(), component_path.clone()),
                            ]
                            .into(),
                        ).mapped(db, member),
                        RewriteNode::interpolate_patched(
                            &format!("\n    $name$: \
                             $component_path$::unsafe_new_component_state::<{CONTRACT_STATE_NAME}>(),\
                             "),
                            &[
                                (
                                    "name".to_string(),
                                    RewriteNode::Copied(member.name(db).as_syntax_node()),
                                ),
                                ("component_path".to_string(), component_path),
                            ]
                            .into(),
                        ).mapped(db, member),
                    ))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

struct SimpleMemberGeneratedCode<'db> {
    struct_code: RewriteNode<'db>,
    struct_code_mut: RewriteNode<'db>,
    init_code: RewriteNode<'db>,
    storage_member: RewriteNode<'db>,
}

/// Returns the relevant code for a substorage storage member.
fn get_simple_member_code<'db>(
    db: &'db dyn Database,
    member: &ast::Member<'db>,
    config: &StorageMemberConfig,
    metadata: &MacroPluginMetadata<'_>,
) -> SimpleMemberGeneratedCode<'db> {
    let member_wrapper_type = RewriteNode::text(
        if matches!(config.kind, StorageMemberKind::SubStorage | StorageMemberKind::Flat) {
            "FlattenedStorage"
        } else {
            "StorageBase"
        },
    );
    let member_visibility = if backwards_compatible_storage(metadata.edition) {
        RewriteNode::text("pub")
    } else {
        RewriteNode::from_ast(&member.visibility(db))
    };
    let member_name = RewriteNode::from_ast_trimmed(&member.name(db));
    let member_selector_name =
        config.rename.as_deref().map_or_else(|| member_name.clone(), RewriteNode::text);
    let patches = [
        ("attributes".to_string(), RewriteNode::from_ast(&member.attributes(db))),
        ("member_visibility".to_string(), member_visibility),
        ("member_wrapper_type".to_string(), member_wrapper_type),
        ("member_name".to_string(), member_name),
        ("member_selector_name".to_string(), member_selector_name),
        ("member_type".to_string(), RewriteNode::from_ast_trimmed(&member.type_clause(db).ty(db))),
    ]
    .into();

    SimpleMemberGeneratedCode {
        struct_code: RewriteNode::interpolate_patched(
            "\n$attributes$    $member_visibility$ $member_name$: \
             starknet::storage::$member_wrapper_type$<$member_type$>,",
            &patches,
        )
        .mapped(db, member),
        struct_code_mut: RewriteNode::interpolate_patched(
            "\n$attributes$    $member_visibility$ $member_name$: \
             starknet::storage::$member_wrapper_type$<starknet::storage::Mutable<$member_type$>>,",
            &patches,
        )
        .mapped(db, member),
        init_code: if member.has_attr(db, SUBSTORAGE_ATTR) || member.has_attr(db, FLAT_ATTR) {
            RewriteNode::interpolate_patched(
                "\n           $member_name$: starknet::storage::FlattenedStorage{},",
                &patches,
            )
        } else {
            RewriteNode::interpolate_patched(
                "\n           $member_name$: starknet::storage::StorageBase{ address: \
                 selector!(\"$member_selector_name$\") },",
                &patches,
            )
        }
        .mapped(db, member),
        storage_member: RewriteNode::interpolate_patched(
            "\n$attributes$        $member_visibility$ $member_name$: $member_type$,",
            &patches,
        )
        .mapped(db, member),
    }
}
