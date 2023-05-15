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
    let mut reads = Vec::new();
    let mut writes = Vec::new();

    for field in struct_ast.members(db).elements(db) {
        let field_name = field.name(db).as_syntax_node().get_text_without_trivia(db);
        let field_type = field.type_clause(db).ty(db).as_syntax_node().get_text_without_trivia(db);

        reads.push(format!(
            "{field_name}: \
             starknet::StorageAccess::<{field_type}>::read_consecutive_internal(address_domain, \
             ref base)?,",
        ));

        writes.push(format!(
            "starknet::StorageAccess::<{field_type}>::write_consecutive_internal(address_domain, \
             ref base, value.{field_name})?;",
        ));
    }

    let sa_impl = formatdoc!(
        "
        impl StorageAccess{struct_name} of starknet::StorageAccess::<{struct_name}> {{
            fn read(address_domain: u32, base: starknet::StorageBaseAddress) -> \
         starknet::SyscallResult<{struct_name}> {{
                let mut base_mut = base;
                StorageAccess{struct_name}::read_consecutive_internal(address_domain, ref base_mut)
            }}
            fn write(address_domain: u32, base: starknet::StorageBaseAddress, value: \
         {struct_name}) -> starknet::SyscallResult<()> {{
                let mut base_mut = base;
                StorageAccess{struct_name}::write_consecutive_internal(address_domain, ref \
         base_mut, value)
            }}
            fn read_consecutive_internal(address_domain: u32, ref base: StorageBaseAddress) -> \
         SyscallResult<{struct_name}> {{
                Result::Ok(
                    {struct_name} {{
                        {reads}
                    }}
                )
            }}
            #[inline(always)]
            fn write_consecutive_internal(address_domain: u32, ref base: StorageBaseAddress, \
         value: {struct_name}) -> SyscallResult<()> {{
                {writes}
                starknet::SyscallResult::Ok(())
            }}
        }}",
        struct_name = struct_ast.name(db).as_syntax_node().get_text_without_trivia(db),
        reads = reads.join("\n                "),
        writes = writes.join("\n        "),
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
