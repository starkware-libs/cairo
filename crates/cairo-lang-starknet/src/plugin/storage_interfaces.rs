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

use super::starknet_module::backwards_compatible_storage;
use super::utils::has_derive;
use super::{
    FLAT_ATTR, STORAGE_ATTR, STORAGE_NODE_ATTR, STORAGE_SUB_POINTERS_ATTR, STORE_TRAIT,
    SUBSTORAGE_ATTR,
};

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
pub struct StorageInterfacesPlugin;

impl MacroPlugin for StorageInterfacesPlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        metadata: &MacroPluginMetadata<'_>,
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
                let (content, code_mappings) =
                    handle_storage_interface_struct(db, &struct_ast, metadata).build();
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
                let queryable_variants_attrs = enum_ast.query_attr(db, STORAGE_SUB_POINTERS_ATTR);
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
                    if extract_single_unnamed_arg(db, arguments.arguments(db)).is_none() {
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
                let (content, code_mappings) = handle_storage_interface_enum(db, &enum_ast);
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
        vec![STORAGE_NODE_ATTR.to_string(), STORAGE_SUB_POINTERS_ATTR.to_string()]
    }

    fn phantom_type_attributes(&self) -> Vec<String> {
        vec![STORAGE_NODE_ATTR.to_string()]
    }
}

/// The different types of storage interfaces that can be generated.
#[derive(Clone)]
enum StorageInterfaceType {
    StorageNode,
    StructSubPointers,
    EnumSubPointers { target_name: String },
    StorageTrait,
}

/// Helper enum to generate the code snippets for the different types of storage interfaces.
struct StorageInterfaceInfo<'a> {
    db: &'a dyn SyntaxGroup,
    node_type: StorageInterfaceType,
    is_mutable: bool,
}

/// Code generation for the different types of storage interfaces.
impl<'a> StorageInterfaceInfo<'a> {
    /// Initializes the storage node info from en enum AST. Only supports sub pointers.
    fn from_enum_ast(
        db: &'a dyn SyntaxGroup,
        enum_ast: &ast::ItemEnum,
        is_mutable: bool,
    ) -> Option<Self> {
        if let Some(attr) = enum_ast.find_attr(db, STORAGE_SUB_POINTERS_ATTR) {
            if let OptionArgListParenthesized::ArgListParenthesized(arguments) = attr.arguments(db)
            {
                if let Some(arg) = extract_single_unnamed_arg(db, arguments.arguments(db)) {
                    let target_name = arg.as_syntax_node().get_text_without_trivia(db);
                    return Some(Self {
                        db,
                        node_type: StorageInterfaceType::EnumSubPointers { target_name },
                        is_mutable,
                    });
                }
            }
        }
        None
    }

    /// Returns the mutable prefix of camelcase names.
    fn mutable_camelcase(&self) -> &'static str {
        if self.is_mutable { "Mut" } else { "" }
    }
    /// Returns the mutable suffix for functions.
    fn mutable_suffix(&self) -> &'static str {
        if self.is_mutable { "_mut" } else { "" }
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
        let mutable_suffix = self.mutable_camelcase();
        match &self.node_type {
            StorageInterfaceType::StorageNode => {
                format!("$object_name$StorageNode{mutable_suffix}")
            }
            StorageInterfaceType::StructSubPointers => {
                format!("$object_name$SubPointers{mutable_suffix}")
            }
            StorageInterfaceType::StorageTrait => {
                format!("$object_name$StorageBase{mutable_suffix}")
            }

            StorageInterfaceType::EnumSubPointers { target_name } => {
                format!("{target_name}{mutable_suffix}")
            }
        }
    }
    /// Returns the name of the trait that the storage node implements.
    fn node_trait_name(&self) -> String {
        let mutable_suffix = self.mutable_camelcase();
        match self.node_type {
            StorageInterfaceType::StorageNode => {
                format!("starknet::storage::StorageNode{mutable_suffix}<$object_name$>")
            }
            StorageInterfaceType::StructSubPointers
            | StorageInterfaceType::EnumSubPointers { .. } => {
                format!("starknet::storage::SubPointers{mutable_suffix}<$object_name$>")
            }
            StorageInterfaceType::StorageTrait => {
                format!("starknet::storage::StorageTrait{mutable_suffix}<$object_name$>")
            }
        }
    }
    /// Returns the name of the associated type of the storage node.
    fn node_type(&self) -> String {
        match self.node_type {
            StorageInterfaceType::StorageNode => "NodeType".to_string(),
            StorageInterfaceType::StructSubPointers
            | StorageInterfaceType::EnumSubPointers { .. } => "SubPointersType".to_string(),
            StorageInterfaceType::StorageTrait => "BaseType".to_string(),
        }
    }
    /// Returns the name of the function that initializes the storage node.
    fn node_init_function_name(&self) -> String {
        let mutable_suffix = self.mutable_suffix();
        match self.node_type {
            StorageInterfaceType::StorageNode => format!("storage_node{mutable_suffix}",),
            StorageInterfaceType::StructSubPointers
            | StorageInterfaceType::EnumSubPointers { .. } => {
                format!("sub_pointers{mutable_suffix}",)
            }
            StorageInterfaceType::StorageTrait => format!("storage{mutable_suffix}",),
        }
    }
    /// Returns the name of the type that the storage node originates from.
    fn originating_type(&self) -> String {
        let mutable_type = self.mutable_type("$object_name$");
        match self.node_type {
            StorageInterfaceType::StorageNode => {
                format!("starknet::storage::StoragePath<{mutable_type}>")
            }
            StorageInterfaceType::StructSubPointers
            | StorageInterfaceType::EnumSubPointers { .. } => {
                format!("starknet::storage::StoragePointer<{mutable_type}>")
            }
            StorageInterfaceType::StorageTrait => {
                format!("starknet::storage::FlattenedStorage<{mutable_type}>")
            }
        }
    }
    /// Returns the name of the generated impl for the storage node.
    fn node_impl_name(&self) -> String {
        let mutable_suffix = self.mutable_camelcase();
        match self.node_type {
            StorageInterfaceType::StorageNode => {
                format!("$object_name$StorageNode{mutable_suffix}Impl")
            }
            StorageInterfaceType::StructSubPointers
            | StorageInterfaceType::EnumSubPointers { .. } => {
                format!("$object_name$SubPointers{mutable_suffix}Impl")
            }
            StorageInterfaceType::StorageTrait => {
                format!("$object_name$Storage{mutable_suffix}Impl")
            }
        }
    }
    /// Returns the type of the members of the storage node generated struct.
    fn generic_node_members_type(&self, member: &impl QueryAttrs) -> String {
        match self.node_type {
            StorageInterfaceType::StorageNode => {
                if member.has_attr(self.db, FLAT_ATTR) {
                    "starknet::storage::StoragePath".to_string()
                } else {
                    "starknet::storage::PendingStoragePath".to_string()
                }
            }
            StorageInterfaceType::StructSubPointers
            | StorageInterfaceType::EnumSubPointers { .. } => {
                "starknet::storage::StoragePointer".to_string()
            }
            StorageInterfaceType::StorageTrait => {
                if member.has_attr(self.db, SUBSTORAGE_ATTR) || member.has_attr(self.db, FLAT_ATTR)
                {
                    "starknet::storage::FlattenedStorage".to_string()
                } else {
                    "starknet::storage::StorageBase".to_string()
                }
            }
        }
    }
    /// Returns the type of the members of the storage node generated struct, with the generic arg.
    fn concrete_node_members_type(&self, member: &impl QueryAttrs) -> String {
        format!("{}<{}>", self.generic_node_members_type(member), self.mutable_type("$field_type$"))
    }
    /// Returns the code that should be added before the initialization of the storage node struct.
    fn node_constructor_prefix_code(&self) -> String {
        match self.node_type {
            StorageInterfaceType::StorageNode | StorageInterfaceType::StorageTrait => {
                "".to_string()
            }
            StorageInterfaceType::StructSubPointers => "
        let base_address = self.__storage_pointer_address__;
        let mut current_offset = self.__storage_pointer_offset__;"
                .to_string(),
            StorageInterfaceType::EnumSubPointers { .. } => {
                " let selector_storage_pointer = starknet::storage::StoragePointer::<felt252>{
            __storage_pointer_address__: self.__storage_pointer_address__,
            __storage_pointer_offset__: self.__storage_pointer_offset__,
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
    fn node_constructor_field_init_code(&self, is_last: bool, member: &impl QueryAttrs) -> String {
        let member_type = self.generic_node_members_type(member);
        match self.node_type {
            StorageInterfaceType::StorageNode => {
                if member.has_attr(self.db, FLAT_ATTR) {
                    return "        let __$field_name$_value__ = self.into();
                    "
                    .to_string();
                }
                "        let __$field_name$_value__ = \
                 starknet::storage::PendingStoragePathTrait::new(
                        @self,
                        selector!(\"$field_name$\")
                    );
                    "
                .to_string()
            }
            StorageInterfaceType::StructSubPointers => {
                let offset_increment = if is_last {
                    "".to_string()
                } else {
                    format!(
                        "current_offset = current_offset + {STORE_TRAIT}::<$field_type$>::size();
"
                    )
                };
                format!(
                    "        let __$field_name$_value__ = {member_type} {{
            __storage_pointer_address__: base_address,
            __storage_pointer_offset__: current_offset,
        }};
        {offset_increment}"
                )
            }
            StorageInterfaceType::StorageTrait => {
                if member.has_attr(self.db, SUBSTORAGE_ATTR) || member.has_attr(self.db, FLAT_ATTR)
                {
                    "        let __$field_name$_value__ = starknet::storage::FlattenedStorage {};
"
                    .to_string()
                } else {
                    "        let __$field_name$_value__ = starknet::storage::StorageBase \
                     {__base_address__: selector!(\"$field_name$\")};
"
                    .to_string()
                }
            }
            StorageInterfaceType::EnumSubPointers { .. } => {
                let node_type_name = self.node_type_name();
                format!(
                    "        $field_index$ => {node_type_name}::$field_name$({member_type} {{
            __storage_pointer_address__: self.__storage_pointer_address__,
            __storage_pointer_offset__: self.__storage_pointer_offset__ + 1,
        }}),
"
                )
            }
        }
    }
}

/// Generate the code for a single interface type.
fn handle_storage_interface_for_interface_type(
    db: &dyn SyntaxGroup,
    struct_ast: &ast::ItemStruct,
    metadata: &MacroPluginMetadata<'_>,
    storage_node_type: StorageInterfaceType,
    builder: &mut PatchBuilder<'_>,
) {
    let storage_node_info =
        StorageInterfaceInfo { db, node_type: storage_node_type.clone(), is_mutable: false };
    // Create Info with type and false for is_mutable
    add_interface_struct_definition(db, builder, struct_ast, &storage_node_info, metadata);
    add_interface_impl(db, builder, struct_ast, &storage_node_info);
    let mutable_storage_node_info =
        StorageInterfaceInfo { db, node_type: storage_node_type, is_mutable: true };
    // Create Info with type and true for is_mutable
    add_interface_struct_definition(db, builder, struct_ast, &mutable_storage_node_info, metadata);
    add_interface_impl(db, builder, struct_ast, &mutable_storage_node_info);
}

/// Adds the storage interface structs (two variants, mutable and immutable) and their constructor
/// impls to the given builder. This function is called from several places:
///  - From this plugin for adding storage nodes, and storage base trait.
///  - From the derive plugin of the `Store` trait which also generates a sub-pointers interface.
///  - From the contract storage plugin, which generates storage base trait.
pub fn handle_storage_interface_struct<'a>(
    db: &'a dyn SyntaxGroup,
    struct_ast: &ast::ItemStruct,
    metadata: &MacroPluginMetadata<'_>,
) -> PatchBuilder<'a> {
    let mut builder = PatchBuilder::new(db, struct_ast);
    // Run for both StorageNode and StorageTrait
    let storage_interface_types = if struct_ast.has_attr(db, STORAGE_NODE_ATTR) {
        vec![StorageInterfaceType::StorageTrait, StorageInterfaceType::StorageNode]
    } else if struct_ast.has_attr(db, STORAGE_ATTR) {
        vec![StorageInterfaceType::StorageTrait]
    } else if has_derive(struct_ast, db, STORE_TRAIT).is_some() {
        vec![StorageInterfaceType::StructSubPointers]
    } else {
        panic!("Invalid storage interface type.");
    };
    for interface_type in storage_interface_types {
        handle_storage_interface_for_interface_type(
            db,
            struct_ast,
            metadata,
            interface_type,
            &mut builder,
        );
    }
    builder
}

/// Adds the storage interface enum and its constructor impl, for enums with sub-pointers.
pub fn handle_storage_interface_enum(
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

/// Generates the struct definition for the storage interface.
fn add_interface_struct_definition(
    db: &dyn SyntaxGroup,
    builder: &mut PatchBuilder<'_>,
    struct_ast: &ast::ItemStruct,
    storage_node_info: &StorageInterfaceInfo<'_>,
    metadata: &MacroPluginMetadata<'_>,
) {
    let struct_name_syntax = struct_ast.name(db).as_syntax_node();

    let node_type_name = storage_node_info.node_type_name();
    let struct_name_rewrite_node = RewriteNode::new_trimmed(struct_name_syntax.clone());
    let struct_visibility = if backwards_compatible_storage(metadata.edition) {
        RewriteNode::text("pub ")
    } else {
        RewriteNode::Trimmed {
            node: struct_ast.visibility(db).as_syntax_node(),
            trim_left: true,
            trim_right: false,
        }
    };
    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "#[derive(Drop, Copy)]
            $struct_visibility$struct {node_type_name} {{
",
        ),
        &[
            ("struct_visibility".to_string(), struct_visibility),
            ("object_name".to_string(), struct_name_rewrite_node.clone()),
        ]
        .into(),
    ));

    for field in struct_ast.members(db).elements(db) {
        let field_name = field.name(db).as_syntax_node();
        let field_type = field.type_clause(db).ty(db).as_syntax_node();
        let concrete_node_members_type = storage_node_info.concrete_node_members_type(&field);
        let field_visibility = if backwards_compatible_storage(metadata.edition) {
            RewriteNode::text("pub ")
        } else {
            RewriteNode::Trimmed {
                node: field.visibility(db).as_syntax_node(),
                trim_left: true,
                trim_right: false,
            }
        };

        builder.add_modified(RewriteNode::interpolate_patched(
            &format!("    $field_visibility$$field_name$: {concrete_node_members_type},\n",),
            &[
                ("field_visibility".to_string(), field_visibility),
                ("field_name".to_string(), RewriteNode::new_trimmed(field_name)),
                ("field_type".to_string(), RewriteNode::new_trimmed(field_type)),
            ]
            .into(),
        ));
    }
    builder.add_str("}\n");
}

/// Generates the impl for the storage interface.
fn add_interface_impl(
    db: &dyn SyntaxGroup,
    builder: &mut PatchBuilder<'_>,
    struct_ast: &ast::ItemStruct,
    storage_node_info: &StorageInterfaceInfo<'_>,
) {
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
                 fn {node_init_function_name}(self: {originating_type}) -> {node_type_name} \
             {{{node_constructor_prefix_code}
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
            &storage_node_info.node_constructor_field_init_code(is_last, field),
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
            "           $field_name$: __$field_name$_value__,\n",
            &[("field_name".to_string(), RewriteNode::new_trimmed(field_name))].into(),
        ));
    }

    builder.add_str("        }\n    }\n}\n");
}

/// Generates the enum definition for an enum with sub pointers.
fn add_node_enum_definition(
    db: &dyn SyntaxGroup,
    builder: &mut PatchBuilder<'_>,
    enum_ast: &ast::ItemEnum,
    is_mutable: bool,
) {
    let storage_node_info = StorageInterfaceInfo::from_enum_ast(db, enum_ast, is_mutable).unwrap();
    let enum_name_syntax = enum_ast.name(db).as_syntax_node();
    let node_type_name = storage_node_info.node_type_name();

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "#[derive(Drop, Copy)]
            enum {node_type_name} {{
                "
        ),
        &[("object_name".to_string(), RewriteNode::new_trimmed(enum_name_syntax.clone()))].into(),
    ));
    for variant in enum_ast.variants(db).elements(db) {
        let concrete_node_members_type = storage_node_info.concrete_node_members_type(&variant);
        let field_name = variant.name(db).as_syntax_node();
        let field_type = match variant.type_clause(db) {
            ast::OptionTypeClause::Empty(_) => RewriteNode::text("()"),
            ast::OptionTypeClause::TypeClause(tc) => {
                RewriteNode::new_trimmed(tc.ty(db).as_syntax_node())
            }
        };

        builder.add_modified(RewriteNode::interpolate_patched(
            &format!("    $field_name$: {concrete_node_members_type},\n",),
            &[
                ("field_name".to_string(), RewriteNode::new_trimmed(field_name)),
                ("field_type".to_string(), field_type),
            ]
            .into(),
        ));
    }
    builder.add_str("}\n");
}

/// Generates the impl for the storage node for an enum with sub pointers.
fn add_node_enum_impl(
    db: &dyn SyntaxGroup,
    builder: &mut PatchBuilder<'_>,
    enum_ast: &ast::ItemEnum,
    is_mutable: bool,
) {
    let storage_node_info = StorageInterfaceInfo::from_enum_ast(db, enum_ast, is_mutable).unwrap();
    let enum_name_syntax = enum_ast.name(db).as_syntax_node();
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
        &[("object_name".to_string(), RewriteNode::new_trimmed(enum_name_syntax.clone()))].into(),
    ));

    let mut default_index = None;
    for (index, variant) in enum_ast.variants(db).elements(db).iter().enumerate() {
        let variant_selector = if variant.attributes(db).has_attr(db, "default") {
            // If there is more than one default variant, a diagnostic is already emitted from
            // derive(Store).
            // TODO(Gil): Consider adding a diagnostic here as well.
            default_index = Some(index);
            0
        } else {
            index + usize::from(default_index.is_none())
        };
        let field_name = variant.name(db).as_syntax_node();
        let field_type = match variant.type_clause(db) {
            ast::OptionTypeClause::Empty(_) => "()".to_string(),
            ast::OptionTypeClause::TypeClause(tc) => {
                tc.ty(db).as_syntax_node().get_text_without_trivia(db)
            }
        };

        builder.add_modified(RewriteNode::interpolate_patched(
            &storage_node_info.node_constructor_field_init_code(false, variant),
            &[
                ("object_name".to_string(), RewriteNode::new_trimmed(enum_name_syntax.clone())),
                ("field_name".to_string(), RewriteNode::new_trimmed(field_name)),
                ("field_type".to_string(), RewriteNode::text(&field_type)),
                ("field_index".to_string(), RewriteNode::text(&variant_selector.to_string())),
            ]
            .into(),
        ));
    }
    if default_index.is_none() {
        builder.add_str("        0 | _ => panic!(\"Invalid selector value\"),\n");
    } else {
        builder.add_str("        _ => panic!(\"Invalid selector value\"),\n");
    }
    builder.add_str("    }\n}\n}\n");
}
