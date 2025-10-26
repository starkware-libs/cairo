use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::{MacroPluginMetadata, PluginDiagnostic};
use cairo_lang_plugins::plugins::utils::{PluginTypeInfo, TypeVariant};
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use cairo_lang_utils::extract_matches;
use indent::indent_by;
use indoc::formatdoc;
use salsa::Database;

use crate::plugin::consts::STORE_TRAIT;
use crate::plugin::storage_interfaces::{
    handle_storage_interface_struct, struct_members_storage_configs,
};

/// Returns the rewrite node for the `#[derive(starknet::Store)]` attribute.
pub fn handle_store_derive<'db>(
    db: &'db dyn Database,
    item_ast: &ast::ModuleItem<'db>,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    metadata: &MacroPluginMetadata<'_>,
) -> Option<RewriteNode<'db>> {
    let info = PluginTypeInfo::new(db, item_ast)?;
    match &info.type_variant {
        TypeVariant::Struct => {
            // In case of a struct, we need to generate the `Store` trait implementation as well as
            // a sub-pointers implementation.
            let store_trait_code = handle_struct_store(&info)?;
            let sub_pointers_code = if !info.members_info.is_empty() {
                let struct_ast = extract_matches!(item_ast, ast::ModuleItem::Struct);
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
        TypeVariant::Enum => handle_enum(db, &info, diagnostics),
    }
}

/// Derives the `Store` trait for structs annotated with `derive(starknet::Store)`.
fn handle_struct_store<'db>(info: &PluginTypeInfo<'db>) -> Option<RewriteNode<'db>> {
    let full_name = &info.full_typename();
    let name = &info.name;
    let mut fields_write_start = Vec::new();
    let mut fields_read_end = Vec::new();
    let mut fields_write_wrapping = Vec::new();
    let mut reads_values = Vec::new();
    let mut reads_values_at_offset = Vec::new();
    let mut writes = Vec::new();
    let mut writes_at_offset = Vec::new();
    let mut sizes = Vec::new();
    for (i, field) in info.members_info.iter().enumerate() {
        let field_name = &field.name;
        let imp = field.impl_name(STORE_TRAIT);
        let wrapper = field.destruct_with();

        fields_write_start.push(format!("{field_name},"));
        fields_read_end.push(format!("{field_name}: {field_name}.value,"));
        fields_write_wrapping
            .push(format!("let {field_name} = {wrapper} {{ value: {field_name} }};"));

        if i == 0 {
            reads_values.push(format!(
                "let {field_name} = {wrapper} {{ value: \
                 {imp}::read(__store_derive_address_domain__, __store_derive_base__)? }};"
            ));
            reads_values_at_offset.push(format!(
                "let {field_name} = {wrapper} {{ value: \
                 {imp}::read_at_offset(__store_derive_address_domain__, __store_derive_base__, \
                 __store_derive_offset__)? }};"
            ));
        } else {
            let subsequent_read = format!(
                "let {field_name} = {wrapper} {{ value: \
                 {imp}::read_at_offset(__store_derive_address_domain__, __store_derive_base__, \
                 __store_derive_current_offset__)? }};"
            );
            reads_values.push(subsequent_read.clone());
            reads_values_at_offset.push(subsequent_read);
        }
        if i < info.members_info.len() - 1 {
            if i == 0 {
                reads_values
                    .push(format!("let mut __store_derive_current_offset__ = {imp}::size();"));
                reads_values_at_offset.push(format!(
                    "let mut __store_derive_current_offset__ = __store_derive_offset__ + \
                     {imp}::size();"
                ));
            } else {
                let subsequent_read = format!("__store_derive_current_offset__ += {imp}::size();");
                reads_values.push(subsequent_read.clone());
                reads_values_at_offset.push(subsequent_read);
            }
        }

        if i == 0 {
            writes.push(format!(
                "{imp}::write(__store_derive_address_domain__, __store_derive_base__, \
                 {field_name}.value)?;"
            ));
            writes_at_offset.push(format!(
                "{imp}::write_at_offset(__store_derive_address_domain__, __store_derive_base__, \
                 __store_derive_offset__, {field_name}.value)?;"
            ));
        } else {
            let subsequent_write = format!(
                "{imp}::write_at_offset(__store_derive_address_domain__, __store_derive_base__, \
                 __store_derive_current_offset__, {field_name}.value)?;"
            );
            writes.push(subsequent_write.clone());
            writes_at_offset.push(subsequent_write);
        }

        if i < info.members_info.len() - 1 {
            if i == 0 {
                writes.push(format!("let mut __store_derive_current_offset__ = {imp}::size();"));
                writes_at_offset.push(format!(
                    "let mut __store_derive_current_offset__ = __store_derive_offset__ + \
                     {imp}::size();"
                ));
            } else {
                let subsequent_write = format!("__store_derive_current_offset__ += {imp}::size();");
                writes.push(subsequent_write.clone());
                writes_at_offset.push(subsequent_write);
            }
        }
        sizes.push(format!("{imp}::size()"));
    }

    let store_impl = formatdoc!(
        "
        {header} {{
            fn read(address_domain: u32, base: starknet::storage_access::StorageBaseAddress) -> \
         starknet::SyscallResult<{full_name}> {{
                let __store_derive_address_domain__ = address_domain;
                let __store_derive_base__ = base;
                {reads_values}
                starknet::SyscallResult::Ok(
                    {name} {{
                        {fields_read_end}
                    }}
                )
            }}
            fn write(address_domain: u32, base: starknet::storage_access::StorageBaseAddress, \
         value: {full_name}) -> starknet::SyscallResult<()> {{
                let __store_derive_address_domain__ = address_domain;
                let __store_derive_base__ = base;
                let {name} {{
                    {fields_write_start}
                }} = value;
                {fields_write_wrapping}
                {writes}
                starknet::SyscallResult::Ok(())
            }}
            fn read_at_offset(address_domain: u32, base: \
         starknet::storage_access::StorageBaseAddress, offset: u8) -> \
         starknet::SyscallResult<{full_name}> {{
                let __store_derive_address_domain__ = address_domain;
                let __store_derive_base__ = base;
                let __store_derive_offset__ = offset;
                {reads_values_at_offset}
                starknet::SyscallResult::Ok(
                    {name} {{
                        {fields_read_end}
                    }}
                )
            }}
            #[inline(always)]
            fn write_at_offset(address_domain: u32, base: \
         starknet::storage_access::StorageBaseAddress, offset: u8, value: {full_name}) -> \
         starknet::SyscallResult<()> {{
                let __store_derive_address_domain__ = address_domain;
                let __store_derive_base__ = base;
                let __store_derive_offset__ = offset;
                let {name} {{
                    {fields_write_start}
                }} = value;
                {fields_write_wrapping}
                {writes_at_offset}
                starknet::SyscallResult::Ok(())
            }}
            #[inline(always)]
            fn size() -> u8 {{
                {sizes}
            }}
        }}
        ",
        header = info.impl_header(STORE_TRAIT, &[STORE_TRAIT, "core::traits::Destruct"]),
        fields_write_start = fields_write_start.join("\n                "),
        fields_write_wrapping = fields_write_wrapping.join("\n                "),
        fields_read_end = fields_read_end.join("\n                "),
        reads_values_at_offset = reads_values_at_offset.join("\n        "),
        reads_values = reads_values.join("\n        "),
        writes = writes.join("\n        "),
        writes_at_offset = writes_at_offset.join("\n        "),
        sizes = if sizes.is_empty() { "0".to_string() } else { sizes.join(" +\n        ") }
    );

    Some(RewriteNode::Text(store_impl))
}

/// Derives the `starknet::Store` trait for enums annotated with `derive(starknet::Store)`.
fn handle_enum<'db>(
    db: &'db dyn Database,
    info: &PluginTypeInfo<'db>,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) -> Option<RewriteNode<'db>> {
    let enum_name = &info.name;
    let full_name = &info.full_typename();
    let mut match_idx = Vec::new();
    let mut match_idx_at_offset = Vec::new();

    let mut match_value = Vec::new();
    let mut match_value_at_offset = Vec::new();

    let mut match_size = "".to_string();

    let mut default_index = None;
    for (i, variant) in info.members_info.iter().enumerate() {
        let indicator = if variant.attributes.has_attr(db, "default") {
            if default_index.is_some() {
                diagnostics.push(PluginDiagnostic::error(
                    variant.attributes.stable_ptr(db),
                    "Multiple variants annotated with `#[default]`".to_string(),
                ));
                return None;
            }
            default_index = Some(i);
            0
        } else {
            i + usize::from(default_index.is_none())
        };
        let variant_name = &variant.name;
        let imp = variant.impl_name(STORE_TRAIT);

        match_idx.push(formatdoc!(
            "{indicator} => {{
                starknet::SyscallResult::Ok(
                    {enum_name}::{variant_name}(
                        {imp}::read_at_offset(address_domain, base, 1_u8)?
                    )
                )
            }}",
        ));
        match_idx_at_offset.push(formatdoc!(
            "{indicator} => {{
                starknet::SyscallResult::Ok(
                    {enum_name}::{variant_name}(
                        {imp}::read_at_offset(address_domain, base, offset + 1_u8)?
                    )
                )
            }}",
        ));
        match_value.push(formatdoc!(
            "{enum_name}::{variant_name}(x) => {{
                {STORE_TRAIT}::write(address_domain, base, {indicator})?;
                {imp}::write_at_offset(address_domain, base, 1_u8, x)?;
            }}"
        ));
        match_value_at_offset.push(formatdoc!(
            "{enum_name}::{variant_name}(x) => {{
                {STORE_TRAIT}::write_at_offset(address_domain, base, offset, {indicator})?;
                {imp}::write_at_offset(address_domain, base, offset + 1_u8, x)?;
            }}"
        ));

        if match_size.is_empty() {
            match_size = format!("{imp}::size()");
        } else {
            match_size = format!("core::cmp::max({imp}::size(), {match_size})");
        }
    }

    let zero_or_none = if default_index.is_some() { "" } else { "0 |" };
    let store_impl = formatdoc!(
        "
        {header} {{
            fn read(address_domain: u32, base: starknet::storage_access::StorageBaseAddress) -> \
         starknet::SyscallResult<{full_name}> {{
                let idx = {STORE_TRAIT}::<felt252>::read(address_domain, base)?;
                match idx {{
                    {match_idx},
                    {zero_or_none} _ => {{
                        starknet::SyscallResult::Err(array!['Unknown enum indicator:', idx])
                    }}
                }}
            }}
            fn write(address_domain: u32, base: starknet::storage_access::StorageBaseAddress, \
         value: {full_name}) -> starknet::SyscallResult<()> {{
                match value {{
                    {match_value}
                }};
                starknet::SyscallResult::Ok(())
            }}
            fn read_at_offset(address_domain: u32, base: \
         starknet::storage_access::StorageBaseAddress, offset: u8) -> \
         starknet::SyscallResult<{full_name}> {{
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
         starknet::storage_access::StorageBaseAddress, offset: u8, value: {full_name}) -> \
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
        header = info.impl_header(STORE_TRAIT, &[STORE_TRAIT, "core::traits::Destruct"]),
        match_idx = indent_by(12, match_idx.join(",\n")),
        match_idx_at_offset = indent_by(12, match_idx_at_offset.join(",\n")),
        match_value = indent_by(12, match_value.join(",\n")),
        match_value_at_offset = indent_by(12, match_value_at_offset.join(",\n")),
    );

    Some(RewriteNode::Text(store_impl))
}
