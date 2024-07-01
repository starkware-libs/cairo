use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use indoc::formatdoc;

use super::{DERIVE_STORAGE_TRAIT, STORAGE_NODE_ATTR, STORAGE_SUB_POINTERS_ATTR, STORE_TRAIT};

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
            ast::ModuleItem::Struct(struct_ast) => {
                let mut diagnostics = vec![];
                let storage_node_attrs = struct_ast.query_attr(db, STORAGE_NODE_ATTR);
                let storage_sub_pointers_attrs =
                    struct_ast.query_attr(db, STORAGE_SUB_POINTERS_ATTR);
                if storage_node_attrs.is_empty() && storage_sub_pointers_attrs.is_empty() {
                    return PluginResult::default();
                }
                if storage_node_attrs.len() > 1 && storage_sub_pointers_attrs.is_empty() {
                    diagnostics.push(PluginDiagnostic::error(
                        struct_ast.as_syntax_node().stable_ptr(),
                        "Multiple storage node attributes are not allowed.".to_string(),
                    ));
                }
                if storage_sub_pointers_attrs.len() > 1 && storage_node_attrs.is_empty() {
                    diagnostics.push(PluginDiagnostic::error(
                        struct_ast.as_syntax_node().stable_ptr(),
                        "Multiple storage sub pointers attributes are not allowed.".to_string(),
                    ));
                }
                if storage_node_attrs.len() + storage_sub_pointers_attrs.len() > 1 {
                    diagnostics.push(PluginDiagnostic::error(
                        struct_ast.as_syntax_node().stable_ptr(),
                        "Storage node and storage sub pointers attributes cannot be used
                together."
                            .to_string(),
                    ));
                }
                if !diagnostics.is_empty() {
                    return PluginResult { code: None, diagnostics, remove_original_item: false };
                }
                handle_storage_node(db, struct_ast)
            }
            _ => PluginResult::default(),
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![STORAGE_NODE_ATTR.to_string(), STORAGE_SUB_POINTERS_ATTR.to_string()]
    }
}

/// The different types of storage nodes that can be generated.
enum StorageNodeType {
    StorageNode,
    SubPointers,
}

/// Helper enum to generate the code snippets for the different types of storage nodes.
struct StorageNodeInfo {
    node_type: StorageNodeType,
    is_mutable: bool,
}

/// Code generation for the different types of storage nodes.
impl StorageNodeInfo {
    fn from_struct_ast(
        db: &dyn SyntaxGroup,
        struct_ast: &ast::ItemStruct,
        is_mutable: bool,
    ) -> Option<Self> {
        if struct_ast.has_attr(db, STORAGE_NODE_ATTR) {
            Some(Self { node_type: StorageNodeType::StorageNode, is_mutable })
        } else if struct_ast.has_attr(db, STORAGE_SUB_POINTERS_ATTR) {
            Some(Self { node_type: StorageNodeType::SubPointers, is_mutable })
        } else {
            None
        }
    }
    /// Returns the mutable prefix of snakecase names.
    fn mutable_snakecase(&self) -> String {
        if self.is_mutable { "mutable_".to_string() } else { "".to_string() }
    }
    /// Returns the mutable prefix of camelcase names.
    fn mutable_camelcase(&self) -> String {
        if self.is_mutable { "Mutable".to_string() } else { "".to_string() }
    }
    /// Returns a mutable type with a given generic arg.
    fn mutable_type(&self, inner_type: &str) -> String {
        if self.is_mutable {
            format!("starknet::storage::Mutable::<{inner_type}>")
        } else {
            inner_type.to_string()
        }
    }
    /// Returns the name of the storage node type of the specific struct.
    fn node_type_name(&self) -> String {
        let mutable_prefix = self.mutable_camelcase();
        match self.node_type {
            StorageNodeType::StorageNode => {
                format!("{mutable_prefix}$struct_name$StorageNode")
            }
            StorageNodeType::SubPointers => {
                format!("{mutable_prefix}$struct_name$SubPointers")
            }
        }
    }
    /// Returns the name of the trait that the storage node implements.
    fn node_trait_name(&self) -> String {
        let mutable_prefix = self.mutable_camelcase();
        match self.node_type {
            StorageNodeType::StorageNode => {
                format!("starknet::storage::{mutable_prefix}StorageNode<$struct_name$>")
            }
            StorageNodeType::SubPointers => {
                format!("starknet::storage::{mutable_prefix}SubPointers<$struct_name$>")
            }
        }
    }
    /// Returns the name of the associated type of the storage node.
    fn node_type(&self) -> String {
        match self.node_type {
            StorageNodeType::StorageNode => "NodeType".to_string(),
            StorageNodeType::SubPointers => "SubPointersType".to_string(),
        }
    }
    /// Returns the name of the function that initializes the storage node.
    fn node_init_function_name(&self) -> String {
        let mutable_prefix = self.mutable_snakecase();
        match self.node_type {
            StorageNodeType::StorageNode => format!("{mutable_prefix}storage_node"),
            StorageNodeType::SubPointers => format!("{mutable_prefix}sub_pointers"),
        }
    }
    /// Returns the name of the type that the storage node originates from.
    fn originating_type(&self) -> String {
        let mutable_type = self.mutable_type("$struct_name$");
        match self.node_type {
            StorageNodeType::StorageNode => {
                format!("starknet::storage::StoragePath<{mutable_type}>")
            }
            StorageNodeType::SubPointers => {
                format!("starknet::storage::StoragePointer<{mutable_type}>")
            }
        }
    }
    /// Returns the attributes that should be added to the impl of the storage node.
    fn node_impl_attributes(&self) -> String {
        match self.node_type {
            StorageNodeType::StorageNode => "".to_string(),
            StorageNodeType::SubPointers => "#[feature(\"derive-storage\")]".to_string(),
        }
    }
    /// Returns the name of the generated impl for the storage node.
    fn node_impl_name(&self) -> String {
        let mutable_prefix = self.mutable_camelcase();
        match self.node_type {
            StorageNodeType::StorageNode => {
                format!("{mutable_prefix}$struct_name$StorageNodeImpl")
            }
            StorageNodeType::SubPointers => {
                format!("{mutable_prefix}$struct_name$SubPointersImpl<+{DERIVE_STORAGE_TRAIT}<$struct_name$>>")
            }
        }
    }
    /// Returns the type of the members of the storage node generated struct.
    fn generic_node_members_type(&self) -> String {
        match self.node_type {
            StorageNodeType::StorageNode => {
                format!("starknet::storage::PendingStoragePath")
            }
            StorageNodeType::SubPointers => {
                format!("starknet::storage::StoragePointer")
            }
        }
    }
    /// Returns the type of the members of the storage node generated struct, with the generic arg.
    fn concrete_node_members_type(&self) -> String {
        format!("{}<{}>", self.generic_node_members_type(), self.mutable_type("$field_type$"))
    }
    /// Returns the code that should be added before the initialization of the storage node struct.
    fn node_constructor_prefix_code(&self) -> String {
        match self.node_type {
            StorageNodeType::StorageNode => "".to_string(),
            StorageNodeType::SubPointers => "        let base_address = self.address;
        let mut current_offset = self.offset;"
                .to_string(),
        }
    }
    /// Returns the code that should be added for the initialization of each field of the storage
    /// node struct.
    fn node_constructor_field_init_code(&self, is_last: bool) -> String {
        let member_type = self.generic_node_members_type();
        match self.node_type {
            StorageNodeType::StorageNode => {
                format!(
                    "        let $field_name$_value = {member_type} {{ 
            hash_state: self.hash_state,
            pending_key: selector!(\"$field_name$\") 
        }};\n"
                )
            }
            StorageNodeType::SubPointers => {
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
    let mut builder = PatchBuilder::new(db, &struct_ast);

    add_node_struct_definition(db, &mut builder, &struct_ast, false);
    add_node_impl(db, &mut builder, &struct_ast, false);
    add_node_struct_definition(db, &mut builder, &struct_ast, true);
    add_node_impl(db, &mut builder, &struct_ast, true);

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

/// Generates the struct definition for the storage node.
fn add_node_struct_definition(
    db: &dyn SyntaxGroup,
    builder: &mut PatchBuilder,
    struct_ast: &ast::ItemStruct,
    is_mutable: bool,
) {
    let storage_node_info = StorageNodeInfo::from_struct_ast(db, &struct_ast, is_mutable).unwrap();
    let struct_name_syntax = struct_ast.name(db).as_syntax_node();

    let node_type_name = storage_node_info.node_type_name();
    let struct_name_rewrite_node = RewriteNode::new_trimmed(struct_name_syntax.clone());
    let concrete_node_members_type = storage_node_info.concrete_node_members_type();

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "#[derive(Drop, Copy)]
            struct {node_type_name} {{
",
        ),
        &[("struct_name".to_string(), struct_name_rewrite_node.clone())].into(),
    ));

    for field in struct_ast.members(db).elements(db) {
        let field_name = field.name(db).as_syntax_node();
        let field_type = field.type_clause(db).ty(db).as_syntax_node();

        builder.add_modified(RewriteNode::interpolate_patched(
            &format!("    $field_name$: {concrete_node_members_type},\n",),
            &[
                ("field_name".to_string(), RewriteNode::new_trimmed(field_name)),
                ("field_type".to_string(), RewriteNode::new_trimmed(field_type)),
            ]
            .into(),
        ));
    }
    builder.add_str("}\n");
}

/// Generates the impl for the storage node.
fn add_node_impl(
    db: &dyn SyntaxGroup,
    builder: &mut PatchBuilder,
    struct_ast: &ast::ItemStruct,
    is_mutable: bool,
) {
    let storage_node_info = StorageNodeInfo::from_struct_ast(db, &struct_ast, is_mutable).unwrap();
    let struct_name_syntax = struct_ast.name(db).as_syntax_node();
    let node_type_name = storage_node_info.node_type_name();
    let node_impl_attributes = storage_node_info.node_impl_attributes();
    let node_impl_name = storage_node_info.node_impl_name();
    let node_trait_name = storage_node_info.node_trait_name();
    let node_type = storage_node_info.node_type();
    let node_init_function_name = storage_node_info.node_init_function_name();
    let originating_type = storage_node_info.originating_type();
    let node_constructor_prefix_code = storage_node_info.node_constructor_prefix_code();

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "{node_impl_attributes}
             impl {node_impl_name} of {node_trait_name} {{
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
        builder.add_modified(RewriteNode::interpolate_patched(
            &storage_node_info.node_constructor_field_init_code(is_last),
            &[
                ("field_name".to_string(), RewriteNode::new_trimmed(field_name)),
                ("field_type".to_string(), RewriteNode::new_trimmed(field_type)),
            ]
            .into(),
        ));
    }

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!("        {} {{\n", storage_node_info.node_type_name()),
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
}
