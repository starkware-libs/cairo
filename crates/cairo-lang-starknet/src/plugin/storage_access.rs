use std::sync::Arc;

use cairo_lang_defs::plugin::{DynGeneratedFileAuxData, PluginGeneratedFile, PluginResult};
use cairo_lang_semantic::plugin::TrivialPluginAuxData;
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use indoc::formatdoc;

/// Derive the `StorageAccess` trait for structs annotated with `derive(starknet::StorageAccess)`.
pub fn handle_struct(db: &dyn SyntaxGroup, struct_ast: ast::ItemStruct) -> PluginResult {
    let mut reads_values = Vec::new();
    let mut reads_fields = Vec::new();
    let mut writes = Vec::new();
    let mut sizes = Vec::new();

    for (i, field) in struct_ast.members(db).elements(db).iter().enumerate() {
        let field_name = field.name(db).as_syntax_node().get_text_without_trivia(db);
        let field_type = field.type_clause(db).ty(db).as_syntax_node().get_text_without_trivia(db);

        reads_values.push(format!(
            "let {field_name} = \
             starknet::StorageAccess::<{field_type}>::read_at_offset_internal(address_domain, \
             base, current_offset)?;"
        ));
        if i < struct_ast.members(db).elements(db).len() - 1 {
            reads_values.push(format!(
                "current_offset += \
                 starknet::StorageAccess::<{field_type}>::size_internal({field_name});"
            ));
        }

        reads_fields.push(format!("{field_name},"));

        writes.push(format!(
            "starknet::StorageAccess::<{field_type}>::write_at_offset_internal(address_domain, \
             base, current_offset, value.{field_name})?;"
        ));
        if i < struct_ast.members(db).elements(db).len() - 1 {
            writes.push(format!(
                "current_offset += \
                 starknet::StorageAccess::<{field_type}>::size_internal(value.{field_name});"
            ));
        }
        sizes.push(format!(
            "starknet::StorageAccess::<{field_type}>::size_internal(value.{field_name})"
        ));
    }

    let sa_impl = formatdoc!(
        "
        impl StorageAccess{struct_name} of starknet::StorageAccess::<{struct_name}> {{
            fn read(address_domain: u32, base: starknet::StorageBaseAddress) -> \
         starknet::SyscallResult<{struct_name}> {{
                StorageAccess{struct_name}::read_at_offset_internal(address_domain, base, 0_u8)
            }}
            fn write(address_domain: u32, base: starknet::StorageBaseAddress, value: \
         {struct_name}) -> starknet::SyscallResult<()> {{
                StorageAccess{struct_name}::write_at_offset_internal(address_domain, base, 0_u8, \
         value)
            }}
            fn read_at_offset_internal(address_domain: u32, base: StorageBaseAddress, offset: u8) \
         -> starknet::SyscallResult<{struct_name}> {{
                let mut current_offset = offset;
                {reads_values}
                starknet::SyscallResult::Ok(
                    {struct_name} {{
                        {reads_fields}
                    }}
                )
            }}
            #[inline(always)]
            fn write_at_offset_internal(address_domain: u32, base: StorageBaseAddress, offset: u8, \
         value: {struct_name}) -> starknet::SyscallResult<()> {{
                let mut current_offset = offset;
                {writes}
                starknet::SyscallResult::Ok(())
            }}
            #[inline(always)]
            fn size_internal(value: {struct_name}) -> u8 {{
                {sizes}
            }}
        }}",
        struct_name = struct_ast.name(db).as_syntax_node().get_text_without_trivia(db),
        reads_values = reads_values.join("\n        "),
        reads_fields = reads_fields.join("\n                "),
        writes = writes.join("\n        "),
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

/// Returns true if the type should be derived as a storage_access.
pub fn derive_storage_access_needed<T: QueryAttrs>(with_attrs: &T, db: &dyn SyntaxGroup) -> bool {
    with_attrs.query_attr(db, "derive").into_iter().any(|attr| {
        let attr = attr.structurize(db);
        for arg in &attr.args {
            let AttributeArg{
                variant: AttributeArgVariant::Unnamed {
                    value: ast::Expr::Path(path),
                    ..
                },
                ..
            } = arg else {
                continue;
            };
            if path.as_syntax_node().get_text_without_trivia(db) == "storage_access::StorageAccess"
            {
                return true;
            }
        }
        false
    })
}
