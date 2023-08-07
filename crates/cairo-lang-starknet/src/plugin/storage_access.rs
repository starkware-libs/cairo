use std::sync::Arc;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::plugin::TrivialPluginAuxData;
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use indent::indent_by;
use indoc::formatdoc;

/// Derive the `Store` trait for structs annotated with `derive(starknet::Store)`.
pub fn handle_struct(db: &dyn SyntaxGroup, struct_ast: ast::ItemStruct) -> PluginResult {
    let mut reads_values = Vec::new();
    let mut reads_values_at_offset = Vec::new();
    let mut reads_fields = Vec::new();
    let mut writes = Vec::new();
    let mut writes_at_offset = Vec::new();
    let mut sizes = Vec::new();

    for (i, field) in struct_ast.members(db).elements(db).iter().enumerate() {
        let field_name = field.name(db).as_syntax_node().get_text_without_trivia(db);
        let field_type = field.type_clause(db).ty(db).as_syntax_node().get_text_without_trivia(db);

        if i == 0 {
            reads_values.push(format!(
                "let {field_name} = starknet::Store::<{field_type}>::read(address_domain, base)?;"
            ));
            reads_values_at_offset.push(format!(
                "let {field_name} = \
                 starknet::Store::<{field_type}>::read_at_offset(address_domain, base, offset)?;"
            ));
        } else {
            let subsequent_read = format!(
                "let {field_name} = \
                 starknet::Store::<{field_type}>::read_at_offset(address_domain, base, \
                 current_offset)?;"
            );
            reads_values.push(subsequent_read.clone());
            reads_values_at_offset.push(subsequent_read);
        }
        if i < struct_ast.members(db).elements(db).len() - 1 {
            if i == 0 {
                reads_values.push(format!(
                    "let mut current_offset = starknet::Store::<{field_type}>::size();"
                ));
                reads_values_at_offset.push(format!(
                    "let mut current_offset = offset + starknet::Store::<{field_type}>::size();"
                ));
            } else {
                let subsequent_read =
                    format!("current_offset += starknet::Store::<{field_type}>::size();");
                reads_values.push(subsequent_read.clone());
                reads_values_at_offset.push(subsequent_read);
            }
        }

        reads_fields.push(format!("{field_name},"));

        if i == 0 {
            writes.push(format!(
                "starknet::Store::<{field_type}>::write(address_domain, base, \
                 value.{field_name})?;"
            ));
            writes_at_offset.push(format!(
                "starknet::Store::<{field_type}>::write_at_offset(address_domain, base, offset, \
                 value.{field_name})?;"
            ));
        } else {
            let subsequent_write = format!(
                "starknet::Store::<{field_type}>::write_at_offset(address_domain, base, \
                 current_offset, value.{field_name})?;"
            );
            writes.push(subsequent_write.clone());
            writes_at_offset.push(subsequent_write);
        }

        if i < struct_ast.members(db).elements(db).len() - 1 {
            if i == 0 {
                writes.push(format!(
                    "let mut current_offset = starknet::Store::<{field_type}>::size();"
                ));
                writes_at_offset.push(format!(
                    "let mut current_offset = offset + starknet::Store::<{field_type}>::size();"
                ));
            } else {
                let subsequent_write =
                    format!("current_offset += starknet::Store::<{field_type}>::size();");
                writes.push(subsequent_write.clone());
                writes_at_offset.push(subsequent_write);
            }
        }
        sizes.push(format!("starknet::Store::<{field_type}>::size()"));
    }

    let sa_impl = formatdoc!(
        "
        impl Store{struct_name} of starknet::Store::<{struct_name}> {{
            fn read(address_domain: u32, base: starknet::StorageBaseAddress) -> \
         starknet::SyscallResult<{struct_name}> {{
                {reads_values}
                starknet::SyscallResult::Ok(
                    {struct_name} {{
                        {reads_fields}
                    }}
                )
            }}
            fn write(address_domain: u32, base: starknet::StorageBaseAddress, value: \
         {struct_name}) -> starknet::SyscallResult<()> {{
                {writes}
                starknet::SyscallResult::Ok(())
            }}
            fn read_at_offset(address_domain: u32, base: starknet::StorageBaseAddress, offset: u8) \
         -> starknet::SyscallResult<{struct_name}> {{
                {reads_values_at_offset}
                starknet::SyscallResult::Ok(
                    {struct_name} {{
                        {reads_fields}
                    }}
                )
            }}
            #[inline(always)]
            fn write_at_offset(address_domain: u32, base: starknet::StorageBaseAddress, offset: \
         u8, value: {struct_name}) -> starknet::SyscallResult<()> {{
                {writes_at_offset}
                starknet::SyscallResult::Ok(())
            }}
            #[inline(always)]
            fn size() -> u8 {{
                {sizes}
            }}
        }}",
        struct_name = struct_ast.name(db).as_syntax_node().get_text_without_trivia(db),
        reads_values_at_offset = reads_values_at_offset.join("\n        "),
        reads_values = reads_values.join("\n        "),
        reads_fields = reads_fields.join("\n                "),
        writes = writes.join("\n        "),
        writes_at_offset = writes_at_offset.join("\n        "),
        sizes = sizes.join(" +\n        ")
    );

    let diagnostics = vec![];

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "storage_access_impl".into(),
            content: sa_impl,
            aux_data: DynGeneratedFileAuxData(Arc::new(TrivialPluginAuxData {})),
        }),
        diagnostics,
        remove_original_item: false,
    }
}

/// Derive the `StorageAccess` trait for structs annotated with `derive(starknet::Store)`.
pub fn handle_enum(db: &dyn SyntaxGroup, enum_ast: ast::ItemEnum) -> PluginResult {
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
                return PluginResult {
                    code: None,
                    diagnostics: vec![PluginDiagnostic {
                        stable_ptr: variant.stable_ptr().untyped(),
                        message: "Multiple variants annotated with `#[default]`".to_string(),
                    }],
                    remove_original_item: false,
                };
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
            "if idx == {indicator} {{
                starknet::SyscallResult::Ok(
                    {enum_name}::{variant_name}(
                        starknet::Store::read_at_offset(address_domain, base, 1_u8)?
                    )
                )
            }}",
        ));
        match_idx_at_offset.push(formatdoc!(
            "if idx == {indicator} {{
                starknet::SyscallResult::Ok(
                    {enum_name}::{variant_name}(
                        starknet::Store::read_at_offset(address_domain, base, offset + 1_u8)?
                    )
                )
            }}",
        ));
        match_value.push(formatdoc!(
            "{enum_name}::{variant_name}(x) => {{
                starknet::Store::write(address_domain, base, {indicator})?;
                starknet::Store::write_at_offset(address_domain, base, 1_u8, x)?;
            }}"
        ));
        match_value_at_offset.push(formatdoc!(
            "{enum_name}::{variant_name}(x) => {{
                starknet::Store::write_at_offset(address_domain, base, offset, {indicator})?;
                starknet::Store::write_at_offset(address_domain, base, offset + 1_u8, x)?;
            }}"
        ));

        if match_size.is_empty() {
            match_size = format!("starknet::Store::<{variant_type}>::size()");
        } else {
            match_size =
                format!("cmp::max(starknet::Store::<{variant_type}>::size(), {match_size})");
        }
    }

    let sa_impl = formatdoc!(
        "
        impl Store{enum_name} of starknet::Store::<{enum_name}> {{
            fn read(address_domain: u32, base: starknet::StorageBaseAddress) -> \
         starknet::SyscallResult<{enum_name}> {{
                let idx = starknet::Store::<felt252>::read(address_domain, base)?;
                {match_idx}
                else {{
                    starknet::SyscallResult::Err(array!['Unknown enum indicator:', idx])
                }}
            }}
            fn write(address_domain: u32, base: starknet::StorageBaseAddress, value: {enum_name}) \
         -> starknet::SyscallResult<()> {{
                match value {{
                    {match_value}
                }};
                starknet::SyscallResult::Ok(())
            }}
            fn read_at_offset(address_domain: u32, base: starknet::StorageBaseAddress, offset: u8) \
         -> starknet::SyscallResult<{enum_name}> {{
                let idx = starknet::Store::<felt252>::read_at_offset(address_domain, base, \
         offset)?;
                {match_idx_at_offset}
                else {{
                    starknet::SyscallResult::Err(array!['Unknown enum indicator:', idx])
                }}
            }}
            #[inline(always)]
            fn write_at_offset(address_domain: u32, base: starknet::StorageBaseAddress, offset: \
         u8, value: {enum_name}) -> starknet::SyscallResult<()> {{
                match value {{
                    {match_value_at_offset}
                }};
                starknet::SyscallResult::Ok(())
            }}
            #[inline(always)]
            fn size() -> u8 {{
                1_u8 + {match_size}
            }}
        }}",
        match_idx = indent_by(8, match_idx.join("\nelse ")),
        match_idx_at_offset = indent_by(8, match_idx_at_offset.join("\nelse ")),
        match_value = indent_by(12, match_value.join(",\n")),
        match_value_at_offset = indent_by(12, match_value_at_offset.join(",\n")),
    );

    let diagnostics = vec![];

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "storage_access_impl".into(),
            content: sa_impl,
            aux_data: DynGeneratedFileAuxData(Arc::new(TrivialPluginAuxData {})),
        }),
        diagnostics,
        remove_original_item: false,
    }
}

/// Returns true if the type should be derived as a storage_access.
pub fn derive_storage_access_needed<T: QueryAttrs>(with_attrs: &T, db: &dyn SyntaxGroup) -> bool {
    with_attrs.query_attr(db, "derive").into_iter().any(|attr| {
        let attr = attr.structurize(db);
        for arg in &attr.args {
            let AttributeArg {
                variant: AttributeArgVariant::Unnamed { value: ast::Expr::Path(path), .. },
                ..
            } = arg
            else {
                continue;
            };
            if path.as_syntax_node().get_text_without_trivia(db) == "starknet::Store" {
                return true;
            }
        }
        false
    })
}
