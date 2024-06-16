use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use indoc::formatdoc;

use super::STORAGE_NODE_ATTR;

/// Generates an impl for the `starknet::StorageNodeTrait` to point to a generate a struct to to be
/// pointed to allowing further access to each of its members (i.e. there will be a fitting member
/// in the inner struct for each member of the struct).
///
/// For example, given the following struct:
/// ```cairo
/// #[starknet::storage_node]
/// struct MyStruct {
///     a: u32,
///     b: u64,
/// }
/// ```
/// The following trait and impl will be generated:
/// ```cairo
/// struct BalancePairStorageNode {
///     balance1: PendingStoragePath<u256>,
///     balance2: PendingStoragePath<felt252>,
/// }
///
/// impl BalancePairStorageNodeTrait of StorageNodeTrait<BalancePair> {
///     type NodeType = BalancePairStorageNode;
///     fn storage_node(self: StoragePath<BalancePair>) -> BalancePairStorageNode {
///         BalancePairStorageNode {
///             balance1: PendingStoragePath {
///                 hash_state: self.hash_state, pending_key: selector!('balance1'),
///             },
///             balance2: PendingStoragePath {
///                 hash_state: self.hash_state, pending_key: selector!('balance2'),
///             },
///         }
///     }
/// }
/// ```
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct StorageNodePlugin;

impl MacroPlugin for StorageNodePlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        match item_ast {
            ast::ModuleItem::Struct(struct_ast) if struct_ast.has_attr(db, STORAGE_NODE_ATTR) => {
                handle_storage_node(db, struct_ast)
            }
            _ => PluginResult::default(),
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![STORAGE_NODE_ATTR.to_string()]
    }
}

/// Generates the trait and impl for a storage node.
fn handle_storage_node(db: &dyn SyntaxGroup, struct_ast: ast::ItemStruct) -> PluginResult {
    // Refactor this to use PatchBuilder
    let struct_name_syntax = struct_ast.name(db).as_syntax_node();
    let mut builder = PatchBuilder::new(db, &struct_ast);

    builder.add_modified(RewriteNode::interpolate_patched(
        "struct $name$StorageNode {\n",
        &[("name".to_string(), RewriteNode::new_trimmed(struct_name_syntax.clone()))].into(),
    ));

    for field in struct_ast.members(db).elements(db) {
        let field_name = field.name(db).as_syntax_node();
        let field_type = field.type_clause(db).ty(db).as_syntax_node();

        builder.add_modified(RewriteNode::interpolate_patched(
            "    $field_name$: starknet::storage::PendingStoragePath<$field_type$>,\n",
            &[
                ("field_name".to_string(), RewriteNode::new_trimmed(field_name)),
                ("field_type".to_string(), RewriteNode::new_trimmed(field_type)),
            ]
            .into(),
        ));
    }

    builder.add_str("}\n");

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "impl $name$StorageNodeTrait of starknet::storage::StorageNodeTrait<$name$> {{
                 type NodeType = $name$StorageNode;
                 fn storage_node(self: starknet::storage::StoragePath<$name$>) -> \
             $name$StorageNode {{
                     $name$StorageNode {{
"
        ),
        &[("name".to_string(), RewriteNode::new_trimmed(struct_name_syntax))].into(),
    ));

    for field in struct_ast.members(db).elements(db) {
        let field_name = field.name(db).as_syntax_node();

        builder.add_modified(RewriteNode::interpolate_patched(
            "           $field_name$: starknet::storage::PendingStoragePath{ hash_state: \
             self.hash_state, pending_key: selector!(\"$field_name$\") },\n",
            &[("field_name".to_string(), RewriteNode::new_trimmed(field_name))].into(),
        ));
    }

    builder.add_str("        }\n    }\n}\n");

    let (content, code_mappings) = builder.build();
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "storage_node".into(),
            content,
            code_mappings,
            aux_data: None,
        }),
        diagnostics: vec![],
        remove_original_item: false,
    }
}
