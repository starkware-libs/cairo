use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::{MacroPluginMetadata, PluginDiagnostic};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use indent::indent_by;
use indoc::formatdoc;

use crate::plugin::consts::STORE_TRAIT;
use crate::plugin::storage_interfaces::{
    handle_storage_interface_struct, struct_members_storage_configs,
};

/// Returns the rewrite node for the `#[derive(starknet::Store)]` attribute.
pub fn handle_store_derive(
    db: &dyn SyntaxGroup,
    item_ast: &ast::ModuleItem,
    diagnostics: &mut Vec<PluginDiagnostic>,
    metadata: &MacroPluginMetadata<'_>,
) -> Option<RewriteNode> {
    match item_ast {
        ast::ModuleItem::Struct(struct_ast) => {
            // In case of a struct, we need to generate the `Store` trait implementation as well as
            // a sub-pointers implementation.
            let store_trait_code = handle_struct_store(db, struct_ast)?;
            let sub_pointers_code = if !struct_ast.members(db).elements(db).is_empty() {
                let configs = struct_members_storage_configs(db, struct_ast, diagnostics);
                handle_storage_interface_struct(db, struct_ast, &configs, metadata)
                    .into_rewrite_node()
            } else {
                RewriteNode::Text("".to_string())
            };
            Some(RewriteNode::interpolate_patched(
                "$store_trait$\n$sub_pointers$",
                &[
                    ("store_trait".to_string(), store_trait_code),
                    ("sub_pointers".to_string(), sub_pointers_code),
                ]
                .into(),
            ))
        }
        ast::ModuleItem::Enum(enum_ast) => handle_enum(db, enum_ast, diagnostics),
        _ => None,
    }
}

/// Derive the `Store` trait for structs annotated with `derive(starknet::Store)`.
fn handle_struct_store(db: &dyn SyntaxGroup, struct_ast: &ast::ItemStruct) -> Option<RewriteNode> {
    let mut fields = Vec::new();
    let mut reads_values = Vec::new();
    let mut reads_values_at_offset = Vec::new();
    let mut writes = Vec::new();
    let mut writes_at_offset = Vec::new();
    let mut sizes = Vec::new();
    for (i, field) in struct_ast.members(db).elements(db).iter().enumerate() {
        let field_name = field.name(db).as_syntax_node().get_text_without_trivia(db);
        let field_type = field.type_clause(db).ty(db).as_syntax_node().get_text_without_trivia(db);

        fields.push(format!("{field_name},"));

        if i == 0 {
            reads_values.push(format!(
                "let {field_name} = \
                 {STORE_TRAIT}::<{field_type}>::read(__store_derive_address_domain__, \
                 __store_derive_base__)?;"
            ));
            reads_values_at_offset.push(format!(
                "let {field_name} = \
                 {STORE_TRAIT}::<{field_type}>::read_at_offset(__store_derive_address_domain__, \
                 __store_derive_base__, __store_derive_offset__)?;"
            ));
        } else {
            let subsequent_read = format!(
                "let {field_name} = \
                 {STORE_TRAIT}::<{field_type}>::read_at_offset(__store_derive_address_domain__, \
                 __store_derive_base__, __store_derive_current_offset__)?;"
            );
            reads_values.push(subsequent_read.clone());
            reads_values_at_offset.push(subsequent_read);
        }
        if i < struct_ast.members(db).elements(db).len() - 1 {
            if i == 0 {
                reads_values.push(format!(
                    "let mut __store_derive_current_offset__ = \
                     {STORE_TRAIT}::<{field_type}>::size();"
                ));
                reads_values_at_offset.push(format!(
                    "let mut __store_derive_current_offset__ = __store_derive_offset__ + \
                     {STORE_TRAIT}::<{field_type}>::size();"
                ));
            } else {
                let subsequent_read = format!(
                    "__store_derive_current_offset__ += {STORE_TRAIT}::<{field_type}>::size();"
                );
                reads_values.push(subsequent_read.clone());
                reads_values_at_offset.push(subsequent_read);
            }
        }

        if i == 0 {
            writes.push(format!(
                "{STORE_TRAIT}::<{field_type}>::write(__store_derive_address_domain__, \
                 __store_derive_base__, {field_name})?;"
            ));
            writes_at_offset.push(format!(
                "{STORE_TRAIT}::<{field_type}>::write_at_offset(__store_derive_address_domain__, \
                 __store_derive_base__, __store_derive_offset__, {field_name})?;"
            ));
        } else {
            let subsequent_write = format!(
                "{STORE_TRAIT}::<{field_type}>::write_at_offset(__store_derive_address_domain__, \
                 __store_derive_base__, __store_derive_current_offset__, {field_name})?;"
            );
            writes.push(subsequent_write.clone());
            writes_at_offset.push(subsequent_write);
        }

        if i < struct_ast.members(db).elements(db).len() - 1 {
            if i == 0 {
                writes.push(format!(
                    "let mut __store_derive_current_offset__ = \
                     {STORE_TRAIT}::<{field_type}>::size();"
                ));
                writes_at_offset.push(format!(
                    "let mut __store_derive_current_offset__ = __store_derive_offset__ + \
                     {STORE_TRAIT}::<{field_type}>::size();"
                ));
            } else {
                let subsequent_write = format!(
                    "__store_derive_current_offset__ += {STORE_TRAIT}::<{field_type}>::size();"
                );
                writes.push(subsequent_write.clone());
                writes_at_offset.push(subsequent_write);
            }
        }
        sizes.push(format!("{STORE_TRAIT}::<{field_type}>::size()"));
    }

    let store_impl = formatdoc!(
        "
        impl Store{struct_name} of {STORE_TRAIT}::<{struct_name}> {{
            fn read(address_domain: u32, base: starknet::storage_access::StorageBaseAddress) -> \
         starknet::SyscallResult<{struct_name}> {{
                let __store_derive_address_domain__ = address_domain;
                let __store_derive_base__ = base;
                {reads_values}
                starknet::SyscallResult::Ok(
                    {struct_name} {{
                        {fields}
                    }}
                )
            }}
            fn write(address_domain: u32, base: starknet::storage_access::StorageBaseAddress, \
         value: {struct_name}) -> starknet::SyscallResult<()> {{
                let __store_derive_address_domain__ = address_domain;
                let __store_derive_base__ = base;
                let {struct_name} {{
                    {fields}
                }} = value;
                {writes}
                starknet::SyscallResult::Ok(())
            }}
            fn read_at_offset(address_domain: u32, base: \
         starknet::storage_access::StorageBaseAddress, offset: u8) -> \
         starknet::SyscallResult<{struct_name}> {{
                let __store_derive_address_domain__ = address_domain;
                let __store_derive_base__ = base;
                let __store_derive_offset__ = offset;
                {reads_values_at_offset}
                starknet::SyscallResult::Ok(
                    {struct_name} {{
                        {fields}
                    }}
                )
            }}
            #[inline(always)]
            fn write_at_offset(address_domain: u32, base: \
         starknet::storage_access::StorageBaseAddress, offset: u8, value: {struct_name}) -> \
         starknet::SyscallResult<()> {{
                let __store_derive_address_domain__ = address_domain;
                let __store_derive_base__ = base;
                let __store_derive_offset__ = offset;
                let {struct_name} {{
                    {fields}
                }} = value;
                {writes_at_offset}
                starknet::SyscallResult::Ok(())
            }}
            #[inline(always)]
            fn size() -> u8 {{
                {sizes}
            }}
        }}
        ",
        struct_name = struct_ast.name(db).as_syntax_node().get_text_without_trivia(db),
        fields = fields.join("\n                "),
        reads_values_at_offset = reads_values_at_offset.join("\n        "),
        reads_values = reads_values.join("\n        "),
        writes = writes.join("\n        "),
        writes_at_offset = writes_at_offset.join("\n        "),
        sizes = if sizes.is_empty() { "0".to_string() } else { sizes.join(" +\n        ") }
    );

    Some(RewriteNode::Text(store_impl))
}

/// Derive the `starknet::Store` trait for enums annotated with `derive(starknet::Store)`.
fn handle_enum(
    db: &dyn SyntaxGroup,
    enum_ast: &ast::ItemEnum,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<RewriteNode> {
    let enum_name = enum_ast.name(db).as_syntax_node().get_text_without_trivia(db);
    let mut match_idx = Vec::new();
    let mut match_idx_at_offset = Vec::new();

    let mut match_value = Vec::new();
    let mut match_value_at_offset = Vec::new();

    let mut match_size = "".to_string();

    let mut default_index = None;
    for (i, variant) in enum_ast.variants(db).elements(db).iter().enumerate() {
        let indicator = if variant.attributes(db).has_attr(db, "default") {
            if default_index.is_some() {
                diagnostics.push(PluginDiagnostic::error(
                    variant.stable_ptr().untyped(),
                    "Multiple variants annotated with `#[default]`".to_string(),
                ));
                return None;
            }
            default_index = Some(i);
            0
        } else {
            i + usize::from(default_index.is_none())
        };
        let variant_name = variant.name(db).text(db);
        let variant_type = match variant.type_clause(db) {
            ast::OptionTypeClause::Empty(_) => "()".to_string(),
            ast::OptionTypeClause::TypeClause(tc) => {
                tc.ty(db).as_syntax_node().get_text_without_trivia(db)
            }
        };

        match_idx.push(formatdoc!(
            "{indicator} => {{
                starknet::SyscallResult::Ok(
                    {enum_name}::{variant_name}(
                        {STORE_TRAIT}::read_at_offset(address_domain, base, 1_u8)?
                    )
                )
            }}",
        ));
        match_idx_at_offset.push(formatdoc!(
            "{indicator} => {{
                starknet::SyscallResult::Ok(
                    {enum_name}::{variant_name}(
                        {STORE_TRAIT}::read_at_offset(address_domain, base, offset + 1_u8)?
                    )
                )
            }}",
        ));
        match_value.push(formatdoc!(
            "{enum_name}::{variant_name}(x) => {{
                {STORE_TRAIT}::write(address_domain, base, {indicator})?;
                {STORE_TRAIT}::write_at_offset(address_domain, base, 1_u8, x)?;
            }}"
        ));
        match_value_at_offset.push(formatdoc!(
            "{enum_name}::{variant_name}(x) => {{
                {STORE_TRAIT}::write_at_offset(address_domain, base, offset, {indicator})?;
                {STORE_TRAIT}::write_at_offset(address_domain, base, offset + 1_u8, x)?;
            }}"
        ));

        if match_size.is_empty() {
            match_size = format!("{STORE_TRAIT}::<{variant_type}>::size()");
        } else {
            match_size =
                format!("core::cmp::max({STORE_TRAIT}::<{variant_type}>::size(), {match_size})");
        }
    }

    let zero_or_none = if default_index.is_some() { "" } else { "0 |" };
    let store_impl = formatdoc!(
        "
        impl Store{enum_name} of {STORE_TRAIT}::<{enum_name}> {{
            fn read(address_domain: u32, base: starknet::storage_access::StorageBaseAddress) -> \
         starknet::SyscallResult<{enum_name}> {{
                let idx = {STORE_TRAIT}::<felt252>::read(address_domain, base)?;
                match idx {{
                    {match_idx},
                    {zero_or_none} _ => {{
                        starknet::SyscallResult::Err(array!['Unknown enum indicator:', idx])
                    }}
                }}
            }}
            fn write(address_domain: u32, base: starknet::storage_access::StorageBaseAddress, \
         value: {enum_name}) -> starknet::SyscallResult<()> {{
                match value {{
                    {match_value}
                }};
                starknet::SyscallResult::Ok(())
            }}
            fn read_at_offset(address_domain: u32, base: \
         starknet::storage_access::StorageBaseAddress, offset: u8) -> \
         starknet::SyscallResult<{enum_name}> {{
                let idx = {STORE_TRAIT}::<felt252>::read_at_offset(address_domain, base, offset)?;
                match idx {{
                    {match_idx_at_offset},
                    {zero_or_none} _ => {{
                        starknet::SyscallResult::Err(array!['Unknown enum indicator:', idx])
                    }}
                }}
            }}
            #[inline(always)]
            fn write_at_offset(address_domain: u32, base: \
         starknet::storage_access::StorageBaseAddress, offset: u8, value: {enum_name}) -> \
         starknet::SyscallResult<()> {{
                match value {{
                    {match_value_at_offset}
                }};
                starknet::SyscallResult::Ok(())
            }}
            #[inline(always)]
            fn size() -> u8 {{
                1_u8 + {match_size}
            }}
        }}
        ",
        match_idx = indent_by(12, match_idx.join(",\n")),
        match_idx_at_offset = indent_by(12, match_idx_at_offset.join(",\n")),
        match_value = indent_by(12, match_value.join(",\n")),
        match_value_at_offset = indent_by(12, match_value_at_offset.join(",\n")),
    );

    Some(RewriteNode::Text(store_impl))
}
