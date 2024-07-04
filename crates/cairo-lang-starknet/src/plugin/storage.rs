use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::{MacroPluginMetadata, PluginDiagnostic};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use indoc::formatdoc;

use super::starknet_module::generation_data::StarknetModuleCommonGenerationData;
use super::starknet_module::{backwards_compatible_storage, StarknetModuleKind};
use super::{CONCRETE_COMPONENT_STATE_NAME, CONTRACT_STATE_NAME, STORAGE_STRUCT_NAME};
use crate::plugin::SUBSTORAGE_ATTR;

/// Generate getters and setters for the members of the storage struct.
pub fn handle_storage_struct(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    struct_ast: ast::ItemStruct,
    starknet_module_kind: StarknetModuleKind,
    data: &mut StarknetModuleCommonGenerationData,
    metadata: &MacroPluginMetadata<'_>,
) {
    let state_struct_name = starknet_module_kind.get_state_struct_name();
    let generic_arg_str = starknet_module_kind.get_generic_arg_str();
    let full_generic_arg_str = starknet_module_kind.get_full_generic_arg_str();
    let full_state_struct_name = starknet_module_kind.get_full_state_struct_name();
    let storage_base_struct_name = starknet_module_kind.get_storage_base_struct_name();
    let storage_base_mut_struct_name = starknet_module_kind.get_storage_base_mut_struct_name();

    let mut members_struct_code = vec![];
    let mut members_struct_code_mut = vec![];
    let mut members_init_code = vec![];
    let mut substorage_members_struct_code = vec![];
    let mut substorage_members_init_code = vec![];
    let mut storage_struct_members = vec![];
    for member in struct_ast.members(db).elements(db) {
        if member.has_attr(db, SUBSTORAGE_ATTR) {
            if let Some((struct_code, init_code)) =
                get_substorage_member_code(db, &member, metadata)
            {
                substorage_members_struct_code.push(struct_code);
                substorage_members_init_code.push(init_code);
            } else {
                diagnostics.push(PluginDiagnostic::error(
                    member.stable_ptr(),
                    format!(
                        "`{SUBSTORAGE_ATTR}` attribute is only allowed for members of type \
                         [some_path::]{STORAGE_STRUCT_NAME}`"
                    ),
                ));
            }
        }
        let SimpleMemberGeneratedCode { struct_code, struct_code_mut, init_code, storage_member } =
            get_simple_member_code(db, &member, metadata);
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
    let storage_struct_visibility = if backwards_compatible_storage(metadata.edition) {
        RewriteNode::text("pub")
    } else {
        RewriteNode::from_ast(&struct_ast.visibility(db))
    };
    data.state_struct_code = RewriteNode::interpolate_patched(
        &formatdoc!(
            "
            {storage_struct_code}
            #[derive(Drop, Copy)]
            $storage_struct_visibility$ struct {storage_base_struct_name} {{$members_struct_code$
            }}
            #[derive(Drop, Copy)]
            $storage_struct_visibility$ struct {storage_base_mut_struct_name} \
             {{$members_struct_code_mut$
            }}
            impl StorageBaseImpl{generic_arg_str} of \
             starknet::storage::StorageBaseTrait<{full_state_struct_name}> {{
                type BaseType = {storage_base_struct_name};
                type BaseMutType = {storage_base_mut_struct_name};
                fn storage_base(self: @{full_state_struct_name}) -> {storage_base_struct_name} {{
                    {storage_base_struct_name} {{$members_init_code$
                    }}
                }}
                fn storage_base_mut(ref self: {full_state_struct_name}) -> \
             {storage_base_mut_struct_name} {{
                    {storage_base_mut_struct_name} {{$members_init_code$
                    }}
                }}
            }}
            pub struct {full_state_struct_name} {{$substorage_members_struct_code$
            }}

            impl {state_struct_name}Drop{generic_arg_str} of Drop<{full_state_struct_name}> {{}}
             
            impl {state_struct_name}Deref{generic_arg_str} of \
             core::ops::SnapshotDeref<{full_state_struct_name}> {{
                type Target = {storage_base_struct_name};
                fn snapshot_deref(self: @{full_state_struct_name}) -> {storage_base_struct_name} {{
                    self.storage_base()
                }}
            }}
            impl {state_struct_name}DerefMut{generic_arg_str} of \
             core::ops::DerefMut<{full_state_struct_name}> {{
                type Target = {storage_base_mut_struct_name};
                fn deref_mut(ref self: {full_state_struct_name}) -> {storage_base_mut_struct_name} \
             {{
                    self.storage_base_mut()
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
            ("storage_struct_visibility".to_string(), storage_struct_visibility),
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
    );
}

/// Returns the relevant code for a substorage storage member.
fn get_substorage_member_code(
    db: &dyn SyntaxGroup,
    member: &ast::Member,
    metadata: &MacroPluginMetadata<'_>,
) -> Option<(RewriteNode, RewriteNode)> {
    let member_visibility = if backwards_compatible_storage(metadata.edition) {
        RewriteNode::text("pub")
    } else {
        RewriteNode::from_ast(&member.visibility(db))
    };
    match member.type_clause(db).ty(db) {
        ast::Expr::Path(type_path) => {
            let elements = &type_path.elements(db);
            // The path has at least one element.
            let (last, path_prefix) = elements.split_last().unwrap();
            match last {
                ast::PathSegment::Simple(segment)
                    if segment.ident(db).text(db) == STORAGE_STRUCT_NAME =>
                {
                    let component_path = RewriteNode::interspersed(
                        path_prefix
                            .iter()
                            .map(|segment| RewriteNode::new_trimmed(segment.as_syntax_node())),
                        RewriteNode::text("::"),
                    );

                    Some((
                        RewriteNode::interpolate_patched(
                            &format!("\n        $member_visibility$ $name$: $component_path$::{CONCRETE_COMPONENT_STATE_NAME},"),
                            &[
                                (
                                    "member_visibility".to_string(),
                                    member_visibility,
                                ),
                                (
                                    "name".to_string(),
                                    RewriteNode::new_trimmed(member.name(db).as_syntax_node()),
                                ),
                                ("component_path".to_string(), component_path.clone()),
                            ]
                            .into(),
                        ),
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
                        ),
                    ))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

struct SimpleMemberGeneratedCode {
    struct_code: RewriteNode,
    struct_code_mut: RewriteNode,
    init_code: RewriteNode,
    storage_member: RewriteNode,
}

/// Returns the relevant code for a substorage storage member.
fn get_simple_member_code(
    db: &dyn SyntaxGroup,
    member: &ast::Member,
    metadata: &MacroPluginMetadata<'_>,
) -> SimpleMemberGeneratedCode {
    let member_name = member.name(db).as_syntax_node();
    let member_type = member.type_clause(db).ty(db).as_syntax_node();
    let member_visibility = if backwards_compatible_storage(metadata.edition) {
        RewriteNode::text("pub")
    } else {
        RewriteNode::from_ast(&member.visibility(db))
    };

    SimpleMemberGeneratedCode {
        struct_code: RewriteNode::interpolate_patched(
            "\n    $member_visibility$ $member_name$: \
             starknet::storage::StorageBase<$member_type$>,",
            &[
                ("member_visibility".to_string(), member_visibility.clone()),
                ("member_name".to_string(), RewriteNode::new_trimmed(member_name.clone())),
                ("member_type".to_string(), RewriteNode::new_trimmed(member_type.clone())),
            ]
            .into(),
        ),
        struct_code_mut: RewriteNode::interpolate_patched(
            "\n    $member_visibility$ $member_name$: \
             starknet::storage::StorageBase<starknet::storage::Mutable<$member_type$>>,",
            &[
                ("member_visibility".to_string(), member_visibility.clone()),
                ("member_name".to_string(), RewriteNode::new_trimmed(member_name.clone())),
                ("member_type".to_string(), RewriteNode::new_trimmed(member_type.clone())),
            ]
            .into(),
        ),
        init_code: RewriteNode::interpolate_patched(
            "\n           $member_name$: starknet::storage::StorageBase{ address: \
             selector!(\"$member_name$\") },",
            &[("member_name".to_string(), RewriteNode::new_trimmed(member_name.clone()))].into(),
        ),
        storage_member: RewriteNode::interpolate_patched(
            "\n          $member_visibility$ $member_name$: $member_type$,",
            &[
                ("member_visibility".to_string(), member_visibility),
                ("member_name".to_string(), RewriteNode::new_trimmed(member_name.clone())),
                ("member_type".to_string(), RewriteNode::new_trimmed(member_type.clone())),
            ]
            .into(),
        ),
    }
}
