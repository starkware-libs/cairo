use std::collections::HashMap;

use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::{ModifiedNode, RewriteNode};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;

use crate::contract::starknet_keccak;

/// Generate getters and setters for the variables in the storage struct.
pub fn handle_storage_struct(
    db: &dyn SyntaxGroup,
    struct_ast: ast::ItemStruct,
    extra_uses_node: &RewriteNode,
) -> (RewriteNode, Vec<PluginDiagnostic>) {
    let mut members_code = Vec::new();
    let mut diagnostics = vec![];

    for member in struct_ast.members(db).elements(db) {
        let name = member.name(db).text(db);
        let address = format!("0x{:x}", starknet_keccak(name.as_bytes()));
        let type_ast = member.type_clause(db).ty(db);
        match try_extract_mapping_types(db, &type_ast) {
            Some((key_type_ast, value_type_ast, MappingType::Legacy)) => {
                members_code.push(RewriteNode::interpolate_patched(
                    handle_legacy_mapping_storage_var(&address).as_str(),
                    HashMap::from([
                        (
                            "storage_var_name".to_string(),
                            RewriteNode::Trimmed(member.name(db).as_syntax_node()),
                        ),
                        ("extra_uses".to_string(), extra_uses_node.clone()),
                        (
                            "key_type".to_string(),
                            RewriteNode::Trimmed(key_type_ast.as_syntax_node()),
                        ),
                        (
                            "value_type".to_string(),
                            RewriteNode::Trimmed(value_type_ast.as_syntax_node()),
                        ),
                    ]),
                ));
            }
            Some((_, _, MappingType::NonLegacy)) => {
                diagnostics.push(PluginDiagnostic {
                    message: "Non `LegacyMap` mapping is not yet supported.".to_string(),
                    stable_ptr: type_ast.stable_ptr().untyped(),
                });
            }
            None => {
                members_code.push(RewriteNode::interpolate_patched(
                    handle_simple_storage_var(&address).as_str(),
                    HashMap::from([
                        (
                            "storage_var_name".to_string(),
                            RewriteNode::Trimmed(member.name(db).as_syntax_node()),
                        ),
                        ("extra_uses".to_string(), extra_uses_node.clone()),
                        ("type_name".to_string(), RewriteNode::Trimmed(type_ast.as_syntax_node())),
                    ]),
                ));
            }
        }
    }
    (RewriteNode::Modified(ModifiedNode { children: members_code }), diagnostics)
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
) -> Option<(ast::Expr, ast::Expr, MappingType)> {
    let as_path = try_extract_matches!(type_ast, ast::Expr::Path)?;
    let [ast::PathSegment::WithGenericArgs(segment)] = &as_path.elements(db)[..] else {
        return None;
    };
    let ty = segment.ident(db).text(db);
    if ty == "LegacyMap" || ty == "Map" {
        let [key_ty, value_ty] =
            <[ast::Expr; 2]>::try_from(segment.generic_args(db).generic_args(db).elements(db))
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
fn handle_simple_storage_var(address: &str) -> String {
    format!(
        "
    mod $storage_var_name$ {{$extra_uses$
        use starknet::SyscallResultTrait;
        use starknet::SyscallResultTraitImpl;

        fn address() -> starknet::StorageBaseAddress {{
            starknet::storage_base_address_const::<{address}>()
        }}
        fn read() -> $type_name$ {{
            // Only address_domain 0 is currently supported.
            let address_domain = 0;
            starknet::StorageAccess::<$type_name$>::read(
                address_domain,
                address(),
            ).unwrap_syscall()
        }}
        fn write(value: $type_name$) {{
            // Only address_domain 0 is currently supported.
            let address_domain = 0;
            starknet::StorageAccess::<$type_name$>::write(
                address_domain,
                address(),
                value,
            ).unwrap_syscall()
        }}
    }}"
    )
}

/// Generate getters and setters skeleton for a non-mapping member in the storage struct.
fn handle_legacy_mapping_storage_var(address: &str) -> String {
    format!(
        "
    mod $storage_var_name$ {{$extra_uses$
        use starknet::SyscallResultTrait;
        use starknet::SyscallResultTraitImpl;

        fn address(key: $key_type$) -> starknet::StorageBaseAddress {{
            starknet::storage_base_address_from_felt(
                hash::LegacyHash::<$key_type$>::hash({address}, key))
        }}
        fn read(key: $key_type$) -> $value_type$ {{
            // Only address_domain 0 is currently supported.
            let address_domain = 0;
            starknet::StorageAccess::<$value_type$>::read(
                address_domain,
                address(key),
            ).unwrap_syscall()
        }}
        fn write(key: $key_type$, value: $value_type$) {{
            // Only address_domain 0 is currently supported.
            let address_domain = 0;
            starknet::StorageAccess::<$value_type$>::write(
                address_domain,
                address(key),
                value,
            ).unwrap_syscall()
        }}
    }}"
    )
}
