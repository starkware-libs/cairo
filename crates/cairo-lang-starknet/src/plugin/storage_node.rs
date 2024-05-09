use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use indoc::formatdoc;

use super::STORAGE_NODE_ATTR;

#[derive(Debug, Default)]
#[non_exhaustive]
/// Generates a trait and impl of updating a storage path of a struct to access each of its members
/// (i.e. there will be a function for each member of the struct).
///
/// For example, given the following struct:
/// ```ignore
/// #[starknet::storage_node]
/// struct MyStruct {
///     a: u32,
///     b: u64,
/// }
/// ```
/// The following trait and impl will be generated:
/// ```ignore
/// struct BalancePairStorageNode {
///     balance1: StoragePath<u256>,
///     balance2: StoragePath<felt252>,
/// }
///
/// impl BalancePairStorageNodeTrait of StructNodeTrait<BalancePair> {
///     type NodeType = BalancePairStorageNode;
///     fn storage_node(self: StoragePath<BalancePair>) -> BalancePairStorageNode {
///         // Should be somehow lazy constructed, i.e. the hash_state should be updated only when
///         // the inner fields are accessed. Can be done if we revert to methods (i.e.`.balance1()`).         
///         BalancePairStorageNode {
///             balance1: storage_path.hash_state.update(selector!('balance1')),
///             balance2: storage_path.hash_state.update(selector!('balance2')),
///         }
///     }
/// }
/// ```
///
/// Notice that `[a_hash]` and `[b_hash]` are the hashes of the field names `a` and `b`
/// respectively, and are calculated at compile time.
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
            "    $field_name$: starknet::storage::StoragePath<$field_type$>,\n",
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
            "impl $name$StorageNodeTrait of starknet::storage::StructNodeTrait<$name$> {{
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
            "           $field_name$: starknet::storage::StoragePath{ hash_state: \
             core::hash::HashStateTrait::update(self.hash_state, selector!(\"$field_name$\")) },\n",
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
