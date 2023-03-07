use std::collections::HashMap;

use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::RewriteNode;
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

        // Attempt to expand the storage interface for a mapping type
        if let Some((key_type_ast, value_type_ast, mapping_type)) =
            try_extract_mapping_types(db, &type_ast)
        {
            match mapping_type {
                MappingType::Legacy => {
                    members_code.push(RewriteNode::interpolate_patched(
                        handle_legacy_mapping_storage_var(&address).as_str(),
                        HashMap::from([
                            (
                                "storage_var_name".to_string(),
                                RewriteNode::new_trimmed(member.name(db).as_syntax_node()),
                            ),
                            ("extra_uses".to_string(), extra_uses_node.clone()),
                            (
                                "key_type".to_string(),
                                RewriteNode::new_trimmed(key_type_ast.as_syntax_node()),
                            ),
                            (
                                "value_type".to_string(),
                                RewriteNode::new_trimmed(value_type_ast.as_syntax_node()),
                            ),
                        ]),
                    ));
                }
                MappingType::NonLegacy => {
                    diagnostics.push(PluginDiagnostic {
                        message: "Non `LegacyMap` mapping is not yet supported.".to_string(),
                        stable_ptr: type_ast.stable_ptr().untyped(),
                    });
                }
            }
        // Attempt to expand the storage interface for an Array type
        } else if let Some(item_type_ast) = try_extract_array_type(db, &type_ast) {
            members_code.push(RewriteNode::interpolate_patched(
                handle_array_storage_var(&address).as_str(),
                HashMap::from([
                    (
                        "storage_var_name".to_string(),
                        RewriteNode::new_trimmed(member.name(db).as_syntax_node()),
                    ),
                    ("extra_uses".to_string(), extra_uses_node.clone()),
                    (
                        "item_type".to_string(),
                        RewriteNode::new_trimmed(item_type_ast.as_syntax_node()),
                    ),
                ]),
            ));
        // Expand the storage interface for a simple type
        } else {
            members_code.push(RewriteNode::interpolate_patched(
                handle_simple_storage_var(&address).as_str(),
                HashMap::from([
                    (
                        "storage_var_name".to_string(),
                        RewriteNode::new_trimmed(member.name(db).as_syntax_node()),
                    ),
                    ("extra_uses".to_string(), extra_uses_node.clone()),
                    ("type_name".to_string(), RewriteNode::new_trimmed(type_ast.as_syntax_node())),
                ]),
            ));
        }
    }
    (RewriteNode::new_modified(members_code), diagnostics)
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

// Given a type, if it is of the form `Array::<T>`, returns T
// Otherwise, returns None.
fn try_extract_array_type(db: &dyn SyntaxGroup, type_ast: &ast::Expr) -> Option<ast::Expr> {
    let as_path = try_extract_matches!(type_ast, ast::Expr::Path)?;
    let [ast::PathSegment::WithGenericArgs(segment)] = &as_path.elements(db)[..] else {
        return None;
    };
    let ty = segment.ident(db).text(db);
    if ty == "Array" {
        let [item_ty] =
            <[ast::Expr; 1]>::try_from(segment.generic_args(db).generic_args(db).elements(db))
                .ok()?;
        Some(item_ty)
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

/// Generate getters and setters skeleton for array in the storage struct.
fn handle_array_storage_var(address: &str) -> String {
    format!(
        "
    mod $storage_var_name$ {{$extra_uses$
        use option::Option;
        use starknet::SyscallResultTrait;
        use starknet::SyscallResultTraitImpl;

        fn length_address() -> starknet::StorageBaseAddress {{
            // TODO(punk5736): Finalize where the array length will be stored. 
            //                 It might be better to calculate the address and feed it into the template.
            starknet::storage_base_address_from_felt(hash::LegacyHash::<(felt, felt)>::hash({address}, ({address}, {address})))
        }}

        fn index_address(index: felt) -> starknet::StorageBaseAddress {{
            // TODO(punk5726): This organizes the elements in the array similar to a map. It would be better to organize
            //                 them continously at `address`, but need to understand how types larger than the word size 
            //                 should be organized.
            starknet::storage_base_address_from_felt(hash::LegacyHash::<felt>::hash({address}, index))
        }}

        fn address() -> starknet::StorageBaseAddress {{
            starknet::storage_base_address_const::<{address}>()
        }}

        fn length() -> felt {{
            let address_domain = 0;
            starknet::StorageAccess::<felt>::read(
                address_domain,
                length_address(),
            ).unwrap_syscall()
        }}

        fn read(index: felt) -> $item_type$ {{
            let address_domain = 0;
            starknet::StorageAccess::<$item_type$>::read(
                address_domain,
                index_address(index)
            ).unwrap_syscall()
        }}

        fn append(value: $item_type$) {{
            // Only address_domain 0 is currently supported.
            let address_domain = 0;

            // Read the length address
            let length = starknet::StorageAccess::<$item_type$>::read(
                address_domain,
                length_address()
            ).unwrap_syscall();

            // Add the element to the array
            starknet::StorageAccess::<$item_type$>::write(
                address_domain,
                index_address(length),
                value,
            ).unwrap_syscall();

            // Increment the length
            starknet::StorageAccess::<$item_type$>::write(
                address_domain,
                length_address(),
                length + 1,
            ).unwrap_syscall()
        }}

        fn pop() -> Option<$item_type$> {{
            // Only address_domain 0 is currently supported.
            let address_domain = 0;

            // Read the length address
            let length = starknet::StorageAccess::<$item_type$>::read(
                address_domain,
                length_address()
            ).unwrap_syscall();

            if length == 0 {{
                return Option::None(());
            }}

            // Read the latest value
            let tail = starknet::StorageAccess::<$item_type$>::read(
                address_domain,
                index_address(length - 1)
            ).unwrap_syscall();

            // Zero out the value
            starknet::StorageAccess::<$item_type$>::write(
                address_domain,
                index_address(length - 1),
                0,
            ).unwrap_syscall();

            // Decrement the length
            starknet::StorageAccess::<$item_type$>::write(
                address_domain,
                length_address(),
                length - 1,
            ).unwrap_syscall();

            Option::Some(tail)
        }}

        fn peek() -> Option<$item_type$> {{
            // Only address_domain 0 is currently supported.
            let address_domain = 0;

            // Read the length address
            let length = starknet::StorageAccess::<$item_type$>::read(
                address_domain,
                length_address()
            ).unwrap_syscall();

            if length == 0 {{
                return Option::None(());
            }}

            // Read the latest value
            Option::Some(starknet::StorageAccess::<$item_type$>::read(
                address_domain,
                index_address(length - 1)
            ).unwrap_syscall())
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
