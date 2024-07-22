use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_defs::plugin_utils::extract_single_unnamed_arg;
use cairo_lang_filesystem::ids::CodeMapping;
use cairo_lang_syntax::node::ast::OptionArgListParenthesized;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use indoc::formatdoc;

use super::utils::{has_derive, AstPathExtract};
use super::{STORAGE_NODE_ATTR, STORE_TRAIT};
use crate::plugin::QUERYABLE_VARIANTS_ATTR;

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
                if storage_node_attrs.is_empty() {
                    return PluginResult::default();
                }
                if storage_node_attrs.len() > 1 {
                    diagnostics.push(PluginDiagnostic::error(
                        struct_ast.as_syntax_node().stable_ptr(),
                        "Multiple storage node attributes are not allowed.".to_string(),
                    ));
                }
                if has_derive(&struct_ast, db, STORE_TRAIT).is_some() {
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
                let (content, code_mappings) = handle_storage_node_struct(db, &struct_ast);
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
            ast::ModuleItem::Enum(enum_ast) => {
                let mut diagnostics = vec![];
                let queryable_variants_attrs = enum_ast.query_attr(db, QUERYABLE_VARIANTS_ATTR);
                if queryable_variants_attrs.is_empty() {
                    return PluginResult::default();
                }
                if queryable_variants_attrs.len() > 1 {
                    diagnostics.push(PluginDiagnostic::error(
                        enum_ast.as_syntax_node().stable_ptr(),
                        "Multiple query variants attributes are not allowed.".to_string(),
                    ));
                }
                if let OptionArgListParenthesized::ArgListParenthesized(arguments) =
                    queryable_variants_attrs[0].arguments(db)
                {
                    if !extract_single_unnamed_arg(db, arguments.arguments(db)).is_some() {
                        diagnostics.push(PluginDiagnostic::error(
                            enum_ast.as_syntax_node().stable_ptr(),
                            "Query variants attribute must have exactly one unnamed argument."
                                .to_string(),
                        ));
                    }
                } else {
                    diagnostics.push(PluginDiagnostic::error(
                        enum_ast.as_syntax_node().stable_ptr(),
                        "Query variants attribute must have exactly one unnamed argument."
                            .to_string(),
                    ));
                }
                if !diagnostics.is_empty() {
                    return PluginResult { code: None, diagnostics, remove_original_item: false };
                }
                let (content, code_mappings) = handle_storage_node_enum(db, &enum_ast);
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
            _ => {
                if item_ast.has_attr(db, STORAGE_NODE_ATTR) {
                    let diagnostics = vec![PluginDiagnostic::error(
                        item_ast.as_syntax_node().stable_ptr(),
                        "#[starknet::storage_node] can only be applied to structs.".to_string(),
                    )];
                    PluginResult { code: None, diagnostics, remove_original_item: false }
                } else {
                    PluginResult::default()
                }
            }
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![STORAGE_NODE_ATTR.to_string(), QUERYABLE_VARIANTS_ATTR.to_string()]
    }

    fn phantom_type_attributes(&self) -> Vec<String> {
        vec![STORAGE_NODE_ATTR.to_string()]
    }
}

/// The different types of storage nodes that can be generated. The plugin exposes only
/// `storage_node` and `queryable_variants`, but the code generation also supports `sub_pointers`,
/// which is being triggered by `#[derive(starknet::Store)]`.
enum StorageNodeType {
    StorageNode,
    SubPointers,
    QueryableVariants(String),
}

/// Helper enum to generate the code snippets for the different types of storage nodes.
struct StorageNodeInfo {
    node_type: StorageNodeType,
    is_mutable: bool,
}

/// Code generation for the different types of storage nodes.
impl StorageNodeInfo {
    /// Initializes the storage node info from en item AST.
    fn from_item_ast(
        db: &dyn SyntaxGroup,
        item_ast: &ast::ModuleItem,
        is_mutable: bool,
    ) -> Option<Self> {
        match item_ast {
            ast::ModuleItem::Struct(struct_ast) => {
                Self::from_struct_ast(db, struct_ast, is_mutable)
            }
            ast::ModuleItem::Enum(enum_ast) => Self::from_enum_ast(db, enum_ast, is_mutable),
            _ => None,
        }
    }

    /// Initializes the storage node info from a struct AST.
    fn from_struct_ast(
        db: &dyn SyntaxGroup,
        struct_ast: &ast::ItemStruct,
        is_mutable: bool,
    ) -> Option<Self> {
        if struct_ast.has_attr(db, STORAGE_NODE_ATTR) {
            Some(Self { node_type: StorageNodeType::StorageNode, is_mutable })
        } else if has_derive(struct_ast, db, STORE_TRAIT).is_some() {
            Some(Self { node_type: StorageNodeType::SubPointers, is_mutable })
        } else {
            None
        }
    }
    /// Initializes the storage node info from en enum AST. Only supports sub pointers.
    fn from_enum_ast(
        db: &dyn SyntaxGroup,
        enum_ast: &ast::ItemEnum,
        is_mutable: bool,
    ) -> Option<Self> {
        if let Some(attr) = enum_ast.find_attr(db, QUERYABLE_VARIANTS_ATTR) {
            if let OptionArgListParenthesized::ArgListParenthesized(arguments) = attr.arguments(db)
            {
                if let Some(arg) = extract_single_unnamed_arg(db, arguments.arguments(db)) {
                    let arg_text = arg.as_syntax_node().get_text_without_trivia(db);
                    return Some(Self {
                        node_type: StorageNodeType::QueryableVariants(arg_text),
                        is_mutable,
                    });
                }
            }
        }
        None
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
        match &self.node_type {
            StorageNodeType::StorageNode => {
                format!("{mutable_prefix}$object_name$StorageNode")
            }
            StorageNodeType::SubPointers => {
                format!("{mutable_prefix}$object_name$SubPointers")
            }
            StorageNodeType::QueryableVariants(enum_name) => {
                format!("{mutable_prefix}{enum_name}")
            }
        }
    }
    /// Returns the name of the trait that the storage node implements.
    fn node_trait_name(&self) -> String {
        let mutable_prefix = self.mutable_camelcase();
        match self.node_type {
            StorageNodeType::StorageNode => {
                format!("starknet::storage::{mutable_prefix}StorageNode<$object_name$>")
            }
            StorageNodeType::SubPointers => {
                format!("starknet::storage::{mutable_prefix}SubPointers<$object_name$>")
            }
            StorageNodeType::QueryableVariants(_) => {
                format!("starknet::storage::{mutable_prefix}QueryableVariants<$object_name$>")
            }
        }
    }
    /// Returns the name of the associated type of the storage node.
    fn node_type(&self) -> String {
        match self.node_type {
            StorageNodeType::StorageNode => "NodeType".to_string(),
            StorageNodeType::SubPointers => "SubPointersType".to_string(),
            StorageNodeType::QueryableVariants(_) => "QueryableVariantsType".to_string(),
        }
    }
    /// Returns the name of the function that initializes the storage node.
    fn node_init_function_name(&self) -> String {
        let mutable_prefix = self.mutable_snakecase();
        match self.node_type {
            StorageNodeType::StorageNode => format!("{mutable_prefix}storage_node"),
            StorageNodeType::SubPointers => {
                format!("{mutable_prefix}sub_pointers")
            }
            StorageNodeType::QueryableVariants(_) => {
                format!("{mutable_prefix}variants")
            }
        }
    }
    /// Returns the name of the type that the storage node originates from.
    fn originating_type(&self) -> String {
        let mutable_type = self.mutable_type("$object_name$");
        match self.node_type {
            StorageNodeType::StorageNode => {
                format!("starknet::storage::StoragePath<{mutable_type}>")
            }
            StorageNodeType::SubPointers | StorageNodeType::QueryableVariants(_) => {
                format!("starknet::storage::StoragePointer<{mutable_type}>")
            }
        }
    }
    /// Returns the name of the generated impl for the storage node.
    fn node_impl_name(&self) -> String {
        let mutable_prefix = self.mutable_camelcase();
        match self.node_type {
            StorageNodeType::StorageNode => {
                format!("{mutable_prefix}$object_name$StorageNodeImpl")
            }
            StorageNodeType::SubPointers => {
                format!("{mutable_prefix}$object_name$SubPointersImpl")
            }
            StorageNodeType::QueryableVariants(_) => {
                format!("{mutable_prefix}$object_name$QueryableVariantsImpl")
            }
        }
    }
    /// Returns the type of the members of the storage node generated struct.
    fn generic_node_members_type(&self) -> String {
        match self.node_type {
            StorageNodeType::StorageNode => "starknet::storage::PendingStoragePath".to_string(),
            StorageNodeType::SubPointers | StorageNodeType::QueryableVariants(_) => {
                "starknet::storage::StoragePointer".to_string()
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
            StorageNodeType::QueryableVariants(_) => {
                "        let selector_storage_pointer = \
                 starknet::storage::StoragePointer::<felt252>{
                                                    address: self.address,
                                                    offset: self.offset,
                                                };
        let selector = \
                 starknet::storage::StoragePointerReadAccess::read(@selector_storage_pointer);
        match selector {
        "
                .to_string()
            }
        }
    }
    /// Returns the code that should be added for the initialization of each field of the storage
    /// node struct.
    fn node_constructor_field_init_code(&self, is_last: bool) -> String {
        let member_type = self.generic_node_members_type();
        match self.node_type {
            StorageNodeType::StorageNode => "        let $field_name$_value = \
                                             starknet::storage::PendingStoragePathTrait::new(
                        @self,
                        selector!(\"$field_name$\")
                    );
                    "
            .to_string(),
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
            StorageNodeType::QueryableVariants(_) => {
                unreachable!("This property is not relevant for enums.")
            }
        }
    }
    /// Returns the code that should be added to the match statement in the sub pointers function of
    /// an enum.
    fn node_constructor_enum_match_code(&self) -> String {
        let member_type = self.generic_node_members_type();
        let node_type_name = self.node_type_name();
        match self.node_type {
            StorageNodeType::QueryableVariants(_) => {
                format!(
                    "   $field_index$ => {node_type_name}::$field_name$({member_type} {{
                address: self.address,
                offset: self.offset + 1,
            }}),
            "
                )
            }
            _ => unreachable!("This property is not relevant for structs."),
        }
    }
}

/// Adds the storage node structs (two variants, mutable and immutable) and their constructor
/// impls to the given builder. Used for both storage nodes and storage sub pointers which are
/// triggered by `#[derive(Store)]`.
pub fn handle_storage_node_struct(
    db: &dyn SyntaxGroup,
    struct_ast: &ast::ItemStruct,
) -> (String, Vec<CodeMapping>) {
    let mut builder = PatchBuilder::new(db, struct_ast);

    add_node_struct_definition(db, &mut builder, struct_ast, false);
    add_node_impl(db, &mut builder, struct_ast, false);
    add_node_struct_definition(db, &mut builder, struct_ast, true);
    add_node_impl(db, &mut builder, struct_ast, true);

    builder.build()
}

pub fn handle_storage_node_enum(
    db: &dyn SyntaxGroup,
    enum_ast: &ast::ItemEnum,
) -> (String, Vec<CodeMapping>) {
    let mut builder = PatchBuilder::new(db, enum_ast);

    add_node_enum_definition(db, &mut builder, enum_ast, false);
    add_node_enum_impl(db, &mut builder, enum_ast, false);
    add_node_enum_definition(db, &mut builder, enum_ast, true);
    add_node_enum_impl(db, &mut builder, enum_ast, true);

    builder.build()
}

/// Generates the struct definition for the storage node.
fn add_node_struct_definition(
    db: &dyn SyntaxGroup,
    builder: &mut PatchBuilder<'_>,
    struct_ast: &ast::ItemStruct,
    is_mutable: bool,
) {
    let storage_node_info = StorageNodeInfo::from_struct_ast(db, struct_ast, is_mutable).unwrap();
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
        &[("object_name".to_string(), struct_name_rewrite_node.clone())].into(),
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
    builder: &mut PatchBuilder<'_>,
    struct_ast: &ast::ItemStruct,
    is_mutable: bool,
) {
    let storage_node_info = StorageNodeInfo::from_struct_ast(db, struct_ast, is_mutable).unwrap();
    let struct_name_syntax = struct_ast.name(db).as_syntax_node();
    let node_type_name = storage_node_info.node_type_name();
    let node_impl_name = storage_node_info.node_impl_name();
    let node_trait_name = storage_node_info.node_trait_name();
    let node_type = storage_node_info.node_type();
    let node_init_function_name = storage_node_info.node_init_function_name();
    let originating_type = storage_node_info.originating_type();
    let node_constructor_prefix_code = storage_node_info.node_constructor_prefix_code();

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "impl {node_impl_name} of {node_trait_name} {{
                 type {node_type} = {node_type_name};
                    fn {node_init_function_name}(self: {originating_type}) -> {node_type_name} {{
                        {node_constructor_prefix_code}
",
        ),
        &[("object_name".to_string(), RewriteNode::new_trimmed(struct_name_syntax.clone()))].into(),
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
        &[("object_name".to_string(), RewriteNode::new_trimmed(struct_name_syntax))].into(),
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

/// How it should look like:
///
/// user defined enum:
/// #[derive(starknet::Store)]
/// enum MyEnum {
///     A: u32,
///     B: u64,
/// }
///
/// generated code:
/// enum MyEnumSubPointers {
///    A: StoragePointer<u32>,
///    B: StoragePointer<u64>,
/// }
///
/// impl MyEnumSubPointers of SubPointers<MyEnum> {
///    type SubPointersType = MyEnumSubPointers;
///    fn sub_pointers(self: StoragePointer<MyEnum>) -> MyEnumSubPointers {
///        let selector_storage_pointer: StoragePointer<felt252> = self.into();
///        let selector = selector_storage_pointer.read();
///         match selector {
///            "0" => MyEnumSubPointers::A(starknet::storage::StoragePointer {
///               address: self.address,
///               offset: self.offset + 1,
///             }
///           "1" => MyEnumSubPointers::B(starknet::storage::StoragePointer {
///              address: self.address,
///              offset: self.offset + 1,
///            }
///       }
///   }


