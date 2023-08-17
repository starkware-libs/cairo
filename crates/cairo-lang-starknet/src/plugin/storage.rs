use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use indoc::formatdoc;

use super::contract::{StarknetModuleCommonGenerationData, StarknetModuleKind};
use crate::contract::starknet_keccak;

/// Generate getters and setters for the variables in the storage struct.
pub fn handle_storage_struct(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    struct_ast: ast::ItemStruct,
    starknet_module_kind: StarknetModuleKind,
    data: &mut StarknetModuleCommonGenerationData,
) {
    let mut members_code = Vec::new();
    let mut members_init_code = Vec::new();
    let mut vars_code = Vec::new();

    let state_struct_name = format!("{}State", starknet_module_kind.to_str_capital());
    let member_state_name = format!("{}MemberState", starknet_module_kind.to_str_capital());
    let (generic_arg_str, full_generic_arg_str) =
        if starknet_module_kind == StarknetModuleKind::Component {
            ("<TCS>", "::<TCS>")
        } else {
            ("", "")
        };
    let full_state_struct_name = format!("{state_struct_name}{generic_arg_str}");

    for member in struct_ast.members(db).elements(db) {
        let name_node = member.name(db).as_syntax_node();
        let name = member.name(db).text(db);
        members_code.push(RewriteNode::interpolate_patched(
            &format!(
                "
        $name$: $name$::{member_state_name},"
            ),
            UnorderedHashMap::from([(
                "name".to_string(),
                RewriteNode::new_trimmed(name_node.clone()),
            )]),
        ));
        members_init_code.push(RewriteNode::interpolate_patched(
            &format!(
                "
            $name$: $name$::{member_state_name}{{}},"
            ),
            UnorderedHashMap::from([("name".to_string(), RewriteNode::new_trimmed(name_node))]),
        ));
        let address = format!("0x{:x}", starknet_keccak(name.as_bytes()));
        let type_ast = member.type_clause(db).ty(db);
        match try_extract_mapping_types(db, &type_ast) {
            Some((key_type_ast, value_type_ast, MappingType::Legacy)) => {
                vars_code.push(RewriteNode::interpolate_patched(
                    handle_legacy_mapping_storage_var(&address, &member_state_name).as_str(),
                    [
                        (
                            "storage_var_name".to_string(),
                            RewriteNode::new_trimmed(member.name(db).as_syntax_node()),
                        ),
                        ("extra_uses".to_string(), data.extra_uses_node.clone()),
                        (
                            "key_type".to_string(),
                            RewriteNode::new_trimmed(key_type_ast.as_syntax_node()),
                        ),
                        (
                            "value_type".to_string(),
                            RewriteNode::new_trimmed(value_type_ast.as_syntax_node()),
                        ),
                    ]
                    .into(),
                ));
            }
            Some((_, _, MappingType::NonLegacy)) => {
                diagnostics.push(PluginDiagnostic {
                    message: "Non `LegacyMap` mapping is not yet supported.".to_string(),
                    stable_ptr: type_ast.stable_ptr().untyped(),
                });
            }
            None => {
                vars_code.push(RewriteNode::interpolate_patched(
                    handle_simple_storage_var(&address, &member_state_name).as_str(),
                    [
                        (
                            "storage_var_name".to_string(),
                            RewriteNode::new_trimmed(member.name(db).as_syntax_node()),
                        ),
                        ("extra_uses".to_string(), data.extra_uses_node.clone()),
                        (
                            "type_name".to_string(),
                            RewriteNode::new_trimmed(type_ast.as_syntax_node()),
                        ),
                    ]
                    .into(),
                ));
            }
        }
    }

    let unsafe_new_function_name =
        format!("unsafe_new_{}_state{generic_arg_str}", starknet_module_kind.to_str_lower());
    let for_testing_function_name =
        format!("{}_state_for_testing{generic_arg_str}", starknet_module_kind.to_str_lower());
    data.state_struct_code = RewriteNode::interpolate_patched(
        formatdoc!(
            "    struct {full_state_struct_name} {{$members_code$
                 }}
                 impl {state_struct_name}Drop{generic_arg_str} of Drop<{full_state_struct_name}> \
             {{}}
                 #[inline(always)]
                 fn {unsafe_new_function_name}() -> {full_state_struct_name} {{
                     {state_struct_name}{full_generic_arg_str} {{$member_init_code$
                     }}
                 }}
                 #[cfg(test)]
                 #[inline(always)]
                 fn {for_testing_function_name}() -> {full_state_struct_name} {{
                     {unsafe_new_function_name}()
                 }}
                 $vars_code$",
        )
        .as_str(),
        UnorderedHashMap::from([
            ("members_code".to_string(), RewriteNode::new_modified(members_code)),
            ("member_init_code".to_string(), RewriteNode::new_modified(members_init_code)),
            ("vars_code".to_string(), RewriteNode::new_modified(vars_code)),
        ]),
    );
}

/// The type of the mapping storage variable.
enum MappingType {
    /// Pedersen based.
    Legacy,
    /// Poseidon based.
    NonLegacy,
}

/// Given a type, if it is of form `Map{Legacy,}::<K, V>`, returns `K` and `V` and the mapping type.
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
    if ty == "LegacyMap" || ty == "Map" {
        let [key_ty, value_ty] = <[ast::GenericArg; 2]>::try_from(
            segment.generic_args(db).generic_args(db).elements(db),
        )
        .ok()?;
        Some((
            key_ty,
            value_ty,
            if ty == "LegacyMap" { MappingType::Legacy } else { MappingType::NonLegacy },
        ))
    } else {
        None
    }
}

/// Generate getters and setters skeleton for a non-mapping member in the storage struct.
fn handle_simple_storage_var(address: &str, member_state_name: &str) -> String {
    format!(
        "
    use $storage_var_name$::Internal{member_state_name}Trait as \
         $storage_var_name${member_state_name}Trait;
    mod $storage_var_name$ {{$extra_uses$
        #[derive(Copy, Drop)]
        struct {member_state_name} {{}}
        trait Internal{member_state_name}Trait {{
            fn address(self: @{member_state_name}) -> starknet::StorageBaseAddress;
            fn read(self: @{member_state_name}) -> $type_name$;
            fn write(ref self: {member_state_name}, value: $type_name$);
        }}

        impl Internal{member_state_name}Impl of Internal{member_state_name}Trait {{
            fn address(self: @{member_state_name}) -> starknet::StorageBaseAddress {{
                starknet::storage_base_address_const::<{address}>()
            }}
            fn read(self: @{member_state_name}) -> $type_name$ {{
                // Only address_domain 0 is currently supported.
                let address_domain = 0_u32;
                starknet::SyscallResultTraitImpl::unwrap_syscall(
                    starknet::Store::<$type_name$>::read(
                        address_domain,
                        self.address(),
                    )
                )
            }}
            fn write(ref self: {member_state_name}, value: $type_name$) {{
                // Only address_domain 0 is currently supported.
                let address_domain = 0_u32;
                starknet::SyscallResultTraitImpl::unwrap_syscall(
                    starknet::Store::<$type_name$>::write(
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

/// Generate getters and setters skeleton for a non-mapping member in the storage struct.
fn handle_legacy_mapping_storage_var(address: &str, member_state_name: &str) -> String {
    format!(
        "
    use $storage_var_name$::Internal{member_state_name}Trait as \
         $storage_var_name${member_state_name}Trait;
    mod $storage_var_name$ {{$extra_uses$
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
                    starknet::Store::<$value_type$>::read(
                        address_domain,
                        self.address(key),
                    )
                )
            }}
            fn write(ref self: {member_state_name}, key: $key_type$, value: $value_type$) {{
                // Only address_domain 0 is currently supported.
                let address_domain = 0_u32;
                starknet::SyscallResultTraitImpl::unwrap_syscall(
                    starknet::Store::<$value_type$>::write(
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
