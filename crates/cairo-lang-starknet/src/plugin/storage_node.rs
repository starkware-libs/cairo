use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use indoc::formatdoc;

use super::{STORAGE_NODE_ATTR, STORAGE_SUB_POINTERS_ATTR, STORE_TRAIT};

/// Generates an impl for the `starknet::StorageNode` to point to a generate a struct to to be
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
/// impl BalancePairStorageNode of StorageNode<BalancePair> {
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
            ast::ModuleItem::Struct(struct_ast)
                if struct_ast.has_attr(db, STORAGE_NODE_ATTR)
                    || struct_ast.has_attr(db, STORAGE_SUB_POINTERS_ATTR) =>
            {
                handle_storage_node(db, struct_ast)
            }
            _ => PluginResult::default(),
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![STORAGE_NODE_ATTR.to_string(), STORAGE_SUB_POINTERS_ATTR.to_string()]
    }
}

/// Helper enum to generate the code snippets for the different types of storage nodes.
enum StorageNodeType {
    StorageNode,
    SubPointers,
}

/// Code generation for the different types of storage nodes.
impl StorageNodeType {
    fn from_struct_ast(db: &dyn SyntaxGroup, struct_ast: &ast::ItemStruct) -> Option<Self> {
        if struct_ast.has_attr(db, STORAGE_NODE_ATTR) {
            Some(Self::StorageNode)
        } else if struct_ast.has_attr(db, STORAGE_SUB_POINTERS_ATTR) {
            Some(Self::SubPointers)
        } else {
            None
        }
    }
    /// Returns the name of the storage node type of the specific struct.
    fn node_type_name(&self) -> String {
        match self {
            Self::StorageNode => "$struct_name$StorageNode".to_string(),
            Self::SubPointers => "$struct_name$SubPointers".to_string(),
        }
    }
    /// Returns the name of the trait that the storage node implements.
    fn node_trait_name(&self) -> String {
        match self {
            Self::StorageNode => "starknet::storage::StorageNode<$struct_name$>".to_string(),
            Self::SubPointers => "starknet::storage::SubPointers<$struct_name$>".to_string(),
        }
    }
    /// Returns the name of the associated type of the storage node.
    fn node_type(&self) -> String {
        match self {
            Self::StorageNode => "NodeType".to_string(),
            Self::SubPointers => "SubPointersType".to_string(),
        }
    }
    /// Returns the name of the function that initializes the storage node.
    fn node_init_function_name(&self) -> String {
        match self {
            Self::StorageNode => "storage_node".to_string(),
            Self::SubPointers => "sub_pointers".to_string(),
        }
    }
    /// Returns the name of the type that the storage node originates from.
    fn originating_type(&self) -> String {
        match self {
            Self::StorageNode => "starknet::storage::StoragePath<$struct_name$>".to_string(),
            Self::SubPointers => "starknet::storage::StoragePointer<$struct_name$>".to_string(),
        }
    }
    /// Returns the name of the generated impl for the storage node.
    fn node_impl_name(&self) -> String {
        match self {
            Self::StorageNode => "$struct_name$StorageNodeImpl".to_string(),
            Self::SubPointers => "$struct_name$SubPointersImpl".to_string(),
        }
    }
    /// Returns the type of the members of the storage node generated struct.
    fn node_members_type(&self) -> String {
        match self {
            Self::StorageNode => "starknet::storage::PendingStoragePath".to_string(),
            Self::SubPointers => "starknet::storage::StoragePointer".to_string(),
        }
    }
    /// Returns the code that should be added before the initialization of the storage node struct.
    fn node_constructor_prefix_code(&self) -> String {
        match self {
            Self::StorageNode => "".to_string(),
            Self::SubPointers => format!(
                "        let base_address = self.address;
        let mut current_offset = self.offset;"
            ),
        }
    }
    /// Returns the code that should be added for the initialization of each field of the storage
    /// node struct.
    fn node_constructor_field_init_code(&self, is_last: bool) -> String {
        match self {
            Self::StorageNode => {
                let member_type = self.node_members_type();
                format!(
                    "        let $field_name$_value = {member_type} {{ 
            hash_state: self.hash_state,
            pending_key: selector!(\"$field_name$\") 
        }};\n"
                )
            }
            Self::SubPointers => {
                let member_type = self.node_members_type();
                let offset_increment = if is_last {
                    "".to_string()
                } else {
                    format!(
                        "current_offset = current_offset + {STORE_TRAIT}::<$field_type$>::size();
"
                    )
                };
                format!(
                    "        let $field_name$_value = {member_type} {{
            address: base_address,
            offset: current_offset,
        }};
        {offset_increment}"
                )
            }
        }
    }
}

/// Generates the trait and impl for a storage node.
fn handle_storage_node(db: &dyn SyntaxGroup, struct_ast: ast::ItemStruct) -> PluginResult {
    let storage_node_type = StorageNodeType::from_struct_ast(db, &struct_ast).unwrap();
    let struct_name_syntax = struct_ast.name(db).as_syntax_node();
    let mut builder = PatchBuilder::new(db, &struct_ast);

    let node_type_name = storage_node_type.node_type_name();
    let struct_name_rewrite_node = RewriteNode::new_trimmed(struct_name_syntax.clone());
    let node_members_type = storage_node_type.node_members_type();
    let node_impl_name = storage_node_type.node_impl_name();
    let node_trait_name = storage_node_type.node_trait_name();
    let node_type = storage_node_type.node_type();
    let node_init_function_name = storage_node_type.node_init_function_name();
    let originating_type = storage_node_type.originating_type();
    let node_constructor_prefix_code = storage_node_type.node_constructor_prefix_code();

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "#[derive(Drop, Copy)]
            struct {node_type_name} {{
",
        ),
        &[("struct_name".to_string(), struct_name_rewrite_node)].into(),
    ));

    for field in struct_ast.members(db).elements(db) {
        let field_name = field.name(db).as_syntax_node();
        let field_type = field.type_clause(db).ty(db).as_syntax_node();

        builder.add_modified(RewriteNode::interpolate_patched(
            &format!("    $field_name$: {node_members_type}<$field_type$>,\n",),
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
            "impl {node_impl_name} of {node_trait_name} {{
                 type {node_type} = {node_type_name};
                    fn {node_init_function_name}(self: {originating_type}) -> {node_type_name} {{
                        {node_constructor_prefix_code}
",
        ),
        &[("struct_name".to_string(), RewriteNode::new_trimmed(struct_name_syntax.clone()))].into(),
    ));

    let fields = struct_ast.members(db).elements(db);
    let mut fields_iter = fields.iter().peekable();
    while let Some(field) = fields_iter.next() {
        let field_name = field.name(db).as_syntax_node();
        let field_type = field.type_clause(db).ty(db).as_syntax_node();
        let is_last = fields_iter.peek().is_none();
        let node_constructor_field_init_code =
            storage_node_type.node_constructor_field_init_code(is_last);

        builder.add_modified(RewriteNode::interpolate_patched(
            &format!("{node_constructor_field_init_code}"),
            &[
                ("field_name".to_string(), RewriteNode::new_trimmed(field_name)),
                ("field_type".to_string(), RewriteNode::new_trimmed(field_type)),
            ]
            .into(),
        ));
    }

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!("        {} {{\n", storage_node_type.node_type_name()),
        &[("struct_name".to_string(), RewriteNode::new_trimmed(struct_name_syntax))].into(),
    ));

    for field in struct_ast.members(db).elements(db) {
        let field_name = field.name(db).as_syntax_node();
        builder.add_modified(RewriteNode::interpolate_patched(
            "           $field_name$: $field_name$_value,\n",
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
