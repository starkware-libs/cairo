use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;
use indoc::formatdoc;

use super::consts::{
    CONCRETE_COMPONENT_STATE_NAME, CONTRACT_STATE_NAME, LEGACY_STORAGE_MAPPING, NESTED_ATTR,
    STORAGE_MAPPING, STORAGE_STRUCT_NAME, STORE_TRAIT,
};
use super::starknet_module::generation_data::StarknetModuleCommonGenerationData;
use super::starknet_module::StarknetModuleKind;
use super::utils::has_v0_attribute;
use crate::contract::starknet_keccak;

/// Generate getters and setters for the members of the storage struct.
pub fn handle_storage_struct(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    struct_ast: ast::ItemStruct,
    starknet_module_kind: StarknetModuleKind,
    data: &mut StarknetModuleCommonGenerationData,
) {
    let mut members_code = Vec::new();
    let mut members_init_code = Vec::new();
    let mut members_module_code = Vec::new();

    let state_struct_name = starknet_module_kind.get_state_struct_name();
    let generic_arg_str = starknet_module_kind.get_generic_arg_str();
    let full_generic_arg_str = starknet_module_kind.get_full_generic_arg_str();
    let full_state_struct_name = starknet_module_kind.get_full_state_struct_name();
    let member_state_name = starknet_module_kind.get_member_state_name();

    for member in struct_ast.members(db).elements(db) {
        let member_code_pieces = get_storage_member_code(
            db,
            diagnostics,
            starknet_module_kind,
            member,
            &member_state_name,
            data.extra_uses_node.clone(),
        );
        if let Some(member_code) = member_code_pieces.member_code {
            members_code.push(member_code);
        }
        if let Some(member_init_code) = member_code_pieces.init_code {
            members_init_code.push(member_init_code);
        }
        if let Some(member_module_code) = member_code_pieces.module_code {
            members_module_code.push(member_module_code);
        }
    }

    let module_kind = starknet_module_kind.to_str_lower();
    let unsafe_new_function_name = format!("unsafe_new_{module_kind}_state");
    data.state_struct_code = RewriteNode::interpolate_patched(
        &formatdoc!(
            "    struct {full_state_struct_name} {{$members_code$
                 }}
                 impl {state_struct_name}Drop{generic_arg_str} of Drop<{full_state_struct_name}> \
             {{}}
                 #[inline(always)]
                 fn {unsafe_new_function_name}{generic_arg_str}() -> {full_state_struct_name} {{
                     {state_struct_name}{full_generic_arg_str} {{$member_init_code$
                     }}
                 }}
                 #[cfg(test)]
                 #[inline(always)]
                 fn {module_kind}_state_for_testing{generic_arg_str}() -> {full_state_struct_name} \
             {{
                     {unsafe_new_function_name}{full_generic_arg_str}()
                 }}
                 $members_module_code$",
        ),
        &[
            ("members_code".to_string(), RewriteNode::new_modified(members_code)),
            ("member_init_code".to_string(), RewriteNode::new_modified(members_init_code)),
            ("members_module_code".to_string(), RewriteNode::new_modified(members_module_code)),
        ]
        .into(),
    );
}

/// The pieces of data required to generate the code relevant for a storage member
#[derive(Default)]
struct StorageMemberCodePieces {
    /// The code to use in the State struct definition.
    member_code: Option<RewriteNode>,
    /// The code to use in the `unsafe_new` function for the initialization of the member of the
    /// State struct.
    init_code: Option<RewriteNode>,
    /// The additional code for the storage member (mainly the related inner module).
    module_code: Option<RewriteNode>,
}

/// Returns the relevant code for a storage member.
fn get_storage_member_code(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    starknet_module_kind: StarknetModuleKind,
    member: ast::Member,
    member_state_name: &String,
    extra_uses_node: RewriteNode,
) -> StorageMemberCodePieces {
    if starknet_module_kind == StarknetModuleKind::Contract
        && has_v0_attribute(db, diagnostics, &member, NESTED_ATTR)
    {
        if let Some((member_code, member_init_code)) = get_nested_storage_member_code(db, &member) {
            return StorageMemberCodePieces {
                member_code: Some(member_code),
                init_code: Some(member_init_code),
                module_code: None,
            };
        } else {
            diagnostics.push(PluginDiagnostic {
                message: format!(
                    "`{NESTED_ATTR}` attribute is only allowed for members of type \
                     [some_path::]{STORAGE_STRUCT_NAME}`"
                ),
                stable_ptr: member.stable_ptr().untyped(),
            });
            return Default::default();
        }
    }

    get_simple_storage_member_code(db, diagnostics, member, member_state_name, extra_uses_node)
}

/// Returns the relevant code for a simple (non-nested) storage member.
fn get_simple_storage_member_code(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    member: ast::Member,
    member_state_name: &String,
    extra_uses_node: RewriteNode,
) -> StorageMemberCodePieces {
    let name_node = member.name(db).as_syntax_node();
    let name = member.name(db).text(db);
    let member_node = RewriteNode::interpolate_patched(
        &format!("$name$: $name$::{member_state_name}"),
        &[("name".to_string(), RewriteNode::new_trimmed(name_node.clone()))].into(),
    );
    let member_code = RewriteNode::interpolate_patched(
        "\n        $member$,",
        &[("member".to_string(), member_node.clone())].into(),
    );

    let member_init_code = RewriteNode::interpolate_patched(
        "\n            $member$ {},",
        &[("member".to_string(), member_node)].into(),
    );
    let address = format!("0x{:x}", starknet_keccak(name.as_bytes()));
    let type_ast = member.type_clause(db).ty(db);

    let member_module_code = match try_extract_mapping_types(db, &type_ast) {
        Some((key_type_ast, value_type_ast, MappingType::Legacy)) => {
            let Some(key_type_path) = get_mapping_full_path_type(db, diagnostics, &key_type_ast)
            else {
                return Default::default();
            };
            let Some(value_type_path) =
                get_mapping_full_path_type(db, diagnostics, &value_type_ast)
            else {
                return Default::default();
            };
            Some(RewriteNode::interpolate_patched(
                &handle_legacy_mapping_storage_member(&address, member_state_name),
                &[
                    (
                        "storage_member_name".to_string(),
                        RewriteNode::new_trimmed(member.name(db).as_syntax_node()),
                    ),
                    ("extra_uses".to_string(), extra_uses_node),
                    ("key_type".to_string(), key_type_path),
                    ("value_type".to_string(), value_type_path),
                ]
                .into(),
            ))
        }
        Some((_, _, MappingType::NonLegacy)) => {
            diagnostics.push(PluginDiagnostic {
                message: format!("Non `{LEGACY_STORAGE_MAPPING}` mapping is not yet supported."),
                stable_ptr: type_ast.stable_ptr().untyped(),
            });
            None
        }
        None => {
            let type_path = get_full_path_type(db, &type_ast);
            Some(RewriteNode::interpolate_patched(
                &handle_simple_storage_member(&address, member_state_name),
                &[
                    (
                        "storage_member_name".to_string(),
                        RewriteNode::new_trimmed(member.name(db).as_syntax_node()),
                    ),
                    ("extra_uses".to_string(), extra_uses_node),
                    ("type_path".to_string(), type_path),
                ]
                .into(),
            ))
        }
    };
    StorageMemberCodePieces {
        member_code: Some(member_code),
        init_code: Some(member_init_code),
        module_code: member_module_code,
    }
}

/// Returns a RewriteNode of the full path of a type for an inner module for a type specified by a
/// mapping generic argument - adds "super::" if needed.
fn get_mapping_full_path_type(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    type_ast: &ast::GenericArg,
) -> Option<RewriteNode> {
    let key_type_path = match type_ast {
        ast::GenericArg::Unnamed(x) => match x.value(db) {
            ast::GenericArgValue::Expr(x) => x.expr(db),
            ast::GenericArgValue::Underscore(_) => {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: type_ast.stable_ptr().untyped(),
                    message: format!(
                        "{LEGACY_STORAGE_MAPPING} generic arguments must be specified"
                    ),
                });
                return None;
            }
        },
        ast::GenericArg::Named(_) => {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: type_ast.stable_ptr().untyped(),
                message: format!("{LEGACY_STORAGE_MAPPING} generic arguments are unnamed"),
            });
            return None;
        }
    };
    Some(get_full_path_type(db, &key_type_path))
}

/// Returns a RewriteNode of the full path of a type for an inner module - adds "super::" if needed.
fn get_full_path_type(db: &dyn SyntaxGroup, type_ast: &ast::Expr) -> RewriteNode {
    let type_node = RewriteNode::new_trimmed(type_ast.as_syntax_node());
    let ast::Expr::Path(type_path) = type_ast else {
        return type_node;
    };
    let first_segment_super = match type_path.elements(db).first().unwrap() {
        ast::PathSegment::Simple(simple) => simple.ident(db).text(db) == "super",
        _ => false,
    };

    if first_segment_super {
        RewriteNode::interpolate_patched(
            "super::$type_path$",
            &[("type_path".to_string(), type_node)].into(),
        )
    } else {
        // No need to add `super::` as this is an absolute type or a locally defined type (which is
        // brought to the inner module by an extra `use`).
        type_node
    }
}

/// Returns the relevant code for a nested storage member.
fn get_nested_storage_member_code(
    db: &dyn SyntaxGroup,
    member: &ast::Member,
) -> Option<(RewriteNode, RewriteNode)> {
    match member.type_clause(db).ty(db) {
        ast::Expr::Path(type_path) => {
            let elements = &type_path.elements(db);
            // The path has at least one element.
            let (last, path_prefix) = elements.split_last().unwrap();
            match last {
                ast::PathSegment::Simple(segment)
                    if segment.ident(db).text(db) == STORAGE_STRUCT_NAME =>
                {
                    let component_path = RewriteNode::new_modified(
                        path_prefix
                            .iter()
                            .flat_map(|segment| {
                                vec![
                                    RewriteNode::new_trimmed(segment.as_syntax_node()),
                                    RewriteNode::Text("::".to_string()),
                                ]
                            })
                            .collect(),
                    );

                    Some((
                        RewriteNode::interpolate_patched(
                            &format!("\n$name$: $component_path${CONCRETE_COMPONENT_STATE_NAME},"),
                            &[
                                (
                                    "name".to_string(),
                                    RewriteNode::Copied(member.name(db).as_syntax_node()),
                                ),
                                ("component_path".to_string(), component_path.clone()),
                            ]
                            .into(),
                        ),
                        RewriteNode::interpolate_patched(
                            &format!("\n    $name$: \
                             $component_path$unsafe_new_component_state::<{CONTRACT_STATE_NAME}>(),\
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

/// The type of the mapping storage member.
enum MappingType {
    /// Pedersen based.
    Legacy,
    /// Poseidon based.
    NonLegacy,
}

/// Given a type, if it is of form `{Legacy,}Map::<K, V>`, returns `K` and `V` and the mapping type.
/// Otherwise, returns None.
fn try_extract_mapping_types(
    db: &dyn SyntaxGroup,
    type_ast: &ast::Expr,
) -> Option<(ast::GenericArg, ast::GenericArg, MappingType)> {
    let as_path = try_extract_matches!(type_ast, ast::Expr::Path)?;
    let [ast::PathSegment::WithGenericArgs(segment)] = &as_path.elements(db)[..] else {
        return None;
    };
    let ty = segment.ident(db).text(db);
    if ty == LEGACY_STORAGE_MAPPING || ty == STORAGE_MAPPING {
        let [key_ty, value_ty] = <[ast::GenericArg; 2]>::try_from(
            segment.generic_args(db).generic_args(db).elements(db),
        )
        .ok()?;
        Some((
            key_ty,
            value_ty,
            if ty == LEGACY_STORAGE_MAPPING { MappingType::Legacy } else { MappingType::NonLegacy },
        ))
    } else {
        None
    }
}

/// Generate getters and setters skeleton for a non-mapping member in the storage struct.
fn handle_simple_storage_member(address: &str, member_state_name: &str) -> String {
    format!(
        "
    impl $storage_member_name$_{member_state_name}Impl = \
         $storage_member_name$::Internal{member_state_name}Impl;
    mod $storage_member_name$ {{$extra_uses$
        #[derive(Copy, Drop)]
        struct {member_state_name} {{}}
        impl Internal{member_state_name}Impl of \
         starknet::Internal{member_state_name}Trait<{member_state_name}, $type_path$> {{
            fn address(self: @{member_state_name}) -> starknet::StorageBaseAddress {{
                starknet::storage_base_address_const::<{address}>()
            }}
            fn read(self: @{member_state_name}) -> $type_path$ {{
                // Only address_domain 0 is currently supported.
                let address_domain = 0_u32;
                starknet::SyscallResultTraitImpl::unwrap_syscall(
                    {STORE_TRAIT}::<$type_path$>::read(
                        address_domain,
                        self.address(),
                    )
                )
            }}
            fn write(ref self: {member_state_name}, value: $type_path$) {{
                // Only address_domain 0 is currently supported.
                let address_domain = 0_u32;
                starknet::SyscallResultTraitImpl::unwrap_syscall(
                    {STORE_TRAIT}::<$type_path$>::write(
                        address_domain,
                        self.address(),
                        value,
                    )
                )
            }}
        }}
    }}"
    )
}

/// Generate getters and setters skeleton for a mapping member in the storage struct.
fn handle_legacy_mapping_storage_member(address: &str, member_state_name: &str) -> String {
    format!(
        "
    use $storage_member_name$::Internal{member_state_name}Trait as \
         $storage_member_name${member_state_name}Trait;
    mod $storage_member_name$ {{$extra_uses$
        #[derive(Copy, Drop)]
        struct {member_state_name} {{}}
        trait Internal{member_state_name}Trait {{
            fn address(self: @{member_state_name}, key: $key_type$) -> \
         starknet::StorageBaseAddress;
            fn read(self: @{member_state_name}, key: $key_type$) -> $value_type$;
            fn write(ref self: {member_state_name}, key: $key_type$, value: $value_type$);
        }}

        impl Internal{member_state_name}Impl of Internal{member_state_name}Trait {{
            fn address(self: @{member_state_name}, key: $key_type$) -> \
         starknet::StorageBaseAddress {{
                starknet::storage_base_address_from_felt252(
                    hash::LegacyHash::<$key_type$>::hash({address}, key))
            }}
            fn read(self: @{member_state_name}, key: $key_type$) -> $value_type$ {{
                // Only address_domain 0 is currently supported.
                let address_domain = 0_u32;
                starknet::SyscallResultTraitImpl::unwrap_syscall(
                    {STORE_TRAIT}::<$value_type$>::read(
                        address_domain,
                        self.address(key),
                    )
                )
            }}
            fn write(ref self: {member_state_name}, key: $key_type$, value: $value_type$) {{
                // Only address_domain 0 is currently supported.
                let address_domain = 0_u32;
                starknet::SyscallResultTraitImpl::unwrap_syscall(
                    {STORE_TRAIT}::<$value_type$>::write(
                        address_domain,
                        self.address(key),
                        value,
                    )
                )
            }}
        }}
    }}"
    )
}
