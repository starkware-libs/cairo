use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::RewriteNode;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use indoc::formatdoc;

use crate::contract::starknet_keccak;

/// Generate getters and setters for the variables in the storage struct.
pub fn handle_storage_struct(
    db: &dyn SyntaxGroup,
    struct_ast: ast::ItemStruct,
    extra_uses_node: &RewriteNode,
    has_event: bool,
) -> (RewriteNode, Vec<PluginDiagnostic>) {
    let mut members_code = Vec::new();
    let mut members_init_code = Vec::new();
    let mut vars_code = Vec::new();
    let mut diagnostics = vec![];

    for member in struct_ast.members(db).elements(db) {
        let name_node = member.name(db).as_syntax_node();
        let name = member.name(db).text(db);
        members_code.push(RewriteNode::interpolate_patched(
            "
        $name$: $name$::Storage,",
            UnorderedHashMap::from([(
                "name".to_string(),
                RewriteNode::new_trimmed(name_node.clone()),
            )]),
        ));
        members_init_code.push(RewriteNode::interpolate_patched(
            "
            $name$: $name$::Storage{},",
            UnorderedHashMap::from([("name".to_string(), RewriteNode::new_trimmed(name_node))]),
        ));
        let address = format!("0x{:x}", starknet_keccak(name.as_bytes()));
        let type_ast = member.type_clause(db).ty(db);
        match try_extract_mapping_types(db, &type_ast) {
            Some((key_type_ast, value_type_ast, MappingType::Legacy)) => {
                vars_code.push(RewriteNode::interpolate_patched(
                    handle_legacy_mapping_storage_var(&address).as_str(),
                    [
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
                    handle_simple_storage_var(&address).as_str(),
                    [
                        (
                            "storage_var_name".to_string(),
                            RewriteNode::new_trimmed(member.name(db).as_syntax_node()),
                        ),
                        ("extra_uses".to_string(), extra_uses_node.clone()),
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
    let empty_event_code =
        if has_event { "" } else { "#[event] #[derive(Drop, starknet::Event)] enum Event {}\n" };
    let storage_code = RewriteNode::interpolate_patched(
        formatdoc!(
            "
            use starknet::event::EventEmitter;
            #[derive(Drop)]
                struct Storage {{$members_code$
                }}
                #[inline(always)]
                fn unsafe_new_storage() -> Storage {{
                    Storage {{$member_init_code$
                    }}
                }}

                $empty_event_code$
                impl StorageEventEmitter of EventEmitter<Storage, Event> {{
                    fn emit(ref self: Storage, event: Event) {{
                        let mut keys = Default::<array::Array>::default();
                        let mut data = Default::<array::Array>::default();
                        starknet::Event::append_keys_and_data(@event, ref keys, ref data);
                        starknet::syscalls::emit_event_syscall(
                            array::ArrayTrait::span(@keys),
                            array::ArrayTrait::span(@data),
                        ).unwrap_syscall()
                    }}
                }}
            $vars_code$
        ",
        )
        .as_str(),
        UnorderedHashMap::from([
            ("members_code".to_string(), RewriteNode::new_modified(members_code)),
            ("vars_code".to_string(), RewriteNode::new_modified(vars_code)),
            ("member_init_code".to_string(), RewriteNode::new_modified(members_init_code)),
            ("empty_event_code".to_string(), RewriteNode::Text(empty_event_code.to_string())),
        ]),
    );
    (storage_code, diagnostics)
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
fn handle_simple_storage_var(address: &str) -> String {
    format!(
        "
    use $storage_var_name$::InternalStorageTrait as $storage_var_name$StorageTrait;
    mod $storage_var_name$ {{$extra_uses$
        use starknet::SyscallResultTrait;
        use starknet::SyscallResultTraitImpl;
        use super;

        #[derive(Copy, Drop)]
        struct Storage {{}}
        trait InternalStorageTrait {{
            fn address(self: @Storage) -> starknet::StorageBaseAddress;
            fn read(self: @Storage) -> $type_name$;
            fn write(ref self: Storage, value: $type_name$);
        }}

        impl InternalStorageImpl of InternalStorageTrait {{
            fn address(self: @Storage) -> starknet::StorageBaseAddress {{
                starknet::storage_base_address_const::<{address}>()
            }}
            fn read(self: @Storage) -> $type_name$ {{
                // Only address_domain 0 is currently supported.
                let address_domain = 0_u32;
                starknet::StorageAccess::<$type_name$>::read(
                    address_domain,
                    self.address(),
                ).unwrap_syscall()
            }}
            fn write(ref self: Storage, value: $type_name$) {{
                // Only address_domain 0 is currently supported.
                let address_domain = 0_u32;
                starknet::StorageAccess::<$type_name$>::write(
                    address_domain,
                    self.address(),
                    value,
                ).unwrap_syscall()
            }}
        }}
    }}"
    )
}

/// Generate getters and setters skeleton for a non-mapping member in the storage struct.
fn handle_legacy_mapping_storage_var(address: &str) -> String {
    format!(
        "
    use $storage_var_name$::InternalStorageTrait as $storage_var_name$StorageTrait;
    mod $storage_var_name$ {{$extra_uses$
        use starknet::SyscallResultTrait;
        use starknet::SyscallResultTraitImpl;
        use super;

        #[derive(Copy, Drop)]
        struct Storage {{}}
        trait InternalStorageTrait {{
            fn address(self: @Storage, key: $key_type$) -> starknet::StorageBaseAddress;
            fn read(self: @Storage, key: $key_type$) -> $value_type$;
            fn write(ref self: Storage, key: $key_type$, value: $value_type$);
        }}

        impl InternalStorageImpl of InternalStorageTrait {{
            fn address(self: @Storage, key: $key_type$) -> starknet::StorageBaseAddress {{
                starknet::storage_base_address_from_felt252(
                    hash::LegacyHash::<$key_type$>::hash({address}, key))
            }}
            fn read(self: @Storage, key: $key_type$) -> $value_type$ {{
                // Only address_domain 0 is currently supported.
                let address_domain = 0_u32;
                starknet::StorageAccess::<$value_type$>::read(
                    address_domain,
                    self.address(key),
                ).unwrap_syscall()
            }}
            fn write(ref self: Storage, key: $key_type$, value: $value_type$) {{
                // Only address_domain 0 is currently supported.
                let address_domain = 0_u32;
                starknet::StorageAccess::<$value_type$>::write(
                    address_domain,
                    self.address(key),
                    value,
                ).unwrap_syscall()
            }}
        }}
    }}"
    )
}
