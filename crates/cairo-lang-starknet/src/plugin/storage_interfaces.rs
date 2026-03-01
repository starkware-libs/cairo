use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_defs::plugin_utils::extract_single_unnamed_arg;
use cairo_lang_filesystem::ids::{CodeMapping, SmolStrId};
use cairo_lang_plugins::plugins::utils::GenericParamsInfo;
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::OptionArgListParenthesized;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use indoc::formatdoc;
use itertools::{Itertools, zip_eq};
use salsa::Database;

use super::starknet_module::backwards_compatible_storage;
use super::utils::{has_derive, validate_v0};
use super::{
    FLAT_ATTR, RENAME_ATTR, STORAGE_ATTR, STORAGE_NODE_ATTR, STORAGE_SUB_POINTERS_ATTR,
    STORE_TRAIT, SUBSTORAGE_ATTR,
};

/// Generates an impl for `starknet::StorageNode` that points to a generated struct allowing
/// further access to each of its members (i.e., there will be a fitting member in the inner struct
/// for each member of the original struct).
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
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        item_ast: ast::ModuleItem<'db>,
        metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult<'db> {
        let mut diagnostics = vec![];
        let storage_node_attrs = item_ast.query_attr(db, STORAGE_NODE_ATTR).collect_vec();
        if !matches!(item_ast, ast::ModuleItem::Struct(_)) && !storage_node_attrs.is_empty() {
            for attr in &storage_node_attrs {
                diagnostics.push(PluginDiagnostic::error(
                    attr.stable_ptr(db),
                    "Can only be applied to structs.".to_string(),
                ));
            }
        }
        let sub_pointers_attrs = item_ast.query_attr(db, STORAGE_SUB_POINTERS_ATTR).collect_vec();
        if !matches!(item_ast, ast::ModuleItem::Enum(_)) && !sub_pointers_attrs.is_empty() {
            for attr in &sub_pointers_attrs {
                diagnostics.push(PluginDiagnostic::error(
                    attr.stable_ptr(db),
                    "Can only be applied to enums.".to_string(),
                ));
            }
        }
        match item_ast {
            ast::ModuleItem::Struct(struct_ast) if !storage_node_attrs.is_empty() => {
                for attr in &storage_node_attrs[1..] {
                    diagnostics.push(PluginDiagnostic::error(
                        attr.stable_ptr(db),
                        "Multiple storage node attributes are not allowed.".to_string(),
                    ));
                }
                if has_derive(&struct_ast, db, STORE_TRAIT).is_some() {
                    diagnostics.push(PluginDiagnostic::error(
                        struct_ast.stable_ptr(db),
                        format!(
                            "Storage node attribute cannot be used with derive of `{STORE_TRAIT}`."
                        ),
                    ));
                }
                if !diagnostics.is_empty() {
                    return PluginResult { code: None, diagnostics, remove_original_item: false };
                }
                let configs = struct_members_storage_configs(db, &struct_ast, &mut diagnostics);
                let (content, code_mappings) =
                    handle_storage_interface_struct(db, &struct_ast, &configs, metadata).build();
                PluginResult {
                    code: Some(PluginGeneratedFile {
                        name: "storage_node".into(),
                        content,
                        code_mappings,
                        aux_data: None,
                        diagnostics_note: Default::default(),
                        is_unhygienic: false,
                    }),
                    diagnostics,
                    remove_original_item: false,
                }
            }
            ast::ModuleItem::Enum(enum_ast) if !sub_pointers_attrs.is_empty() => {
                for attr in &sub_pointers_attrs[1..] {
                    diagnostics.push(PluginDiagnostic::error(
                        attr.stable_ptr(db),
                        "Multiple sub pointers attributes are not allowed.".to_string(),
                    ));
                }
                let arguments = sub_pointers_attrs[0].arguments(db);
                if !matches!(&arguments, OptionArgListParenthesized::ArgListParenthesized(args)
                            if extract_single_unnamed_arg(db, args.arguments(db)).is_some())
                {
                    diagnostics.push(PluginDiagnostic::error(
                        arguments.stable_ptr(db),
                        "Sub pointers attribute must have exactly one unnamed argument."
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
                        diagnostics_note: Default::default(),
                        is_unhygienic: false,
                    }),
                    diagnostics: vec![],
                    remove_original_item: false,
                }
            }
            _ => PluginResult { code: None, diagnostics, remove_original_item: false },
        }
    }

    fn declared_attributes<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        vec![
            SmolStrId::from(db, STORAGE_NODE_ATTR),
            SmolStrId::from(db, STORAGE_SUB_POINTERS_ATTR),
            SmolStrId::from(db, RENAME_ATTR),
        ]
    }

    fn phantom_type_attributes<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        vec![SmolStrId::from(db, STORAGE_NODE_ATTR)]
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
struct StorageInterfaceInfo<'db> {
    db: &'db dyn Database,
    node_type: StorageInterfaceType,
    is_mutable: bool,
}

/// Code generation for the different types of storage interfaces.
impl<'db> StorageInterfaceInfo<'db> {
    /// Initializes the storage node info from an enum AST. Only supports sub pointers.
    fn from_enum_ast(
        db: &'db dyn Database,
        enum_ast: &ast::ItemEnum<'db>,
        is_mutable: bool,
    ) -> Option<Self> {
        if let Some(attr) = enum_ast.find_attr(db, STORAGE_SUB_POINTERS_ATTR)
            && let OptionArgListParenthesized::ArgListParenthesized(arguments) = attr.arguments(db)
            && let Some(arg) = extract_single_unnamed_arg(db, arguments.arguments(db))
        {
            let target_name = arg.as_syntax_node().get_text_without_trivia(db).to_string(db);
            return Some(Self {
                db,
                node_type: StorageInterfaceType::EnumSubPointers { target_name },
                is_mutable,
            });
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
                format!(
                    "starknet::storage::StorageNode{mutable_suffix}<$object_name$$generic_args$>"
                )
            }
            StorageInterfaceType::StructSubPointers
            | StorageInterfaceType::EnumSubPointers { .. } => {
                format!(
                    "starknet::storage::SubPointers{mutable_suffix}<$object_name$$generic_args$>"
                )
            }
            StorageInterfaceType::StorageTrait => {
                format!(
                    "starknet::storage::StorageTrait{mutable_suffix}<$object_name$$generic_args$>"
                )
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
        let mutable_type = self.mutable_type("$object_name$$generic_args$");
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
    fn generic_node_members_type(&self, member: &impl QueryAttrs<'db>) -> String {
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
    fn concrete_node_members_type(&self, member: &impl QueryAttrs<'db>) -> String {
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
                "let selector_storage_pointer = starknet::storage::StoragePointer::<felt252>{
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
    fn node_constructor_field_init_code(
        &self,
        is_last: bool,
        member: &impl QueryAttrs<'db>,
    ) -> String {
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
                        selector!(\"$field_selector_name$\")
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
                     {__base_address__: selector!(\"$field_selector_name$\")};
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
fn handle_storage_interface_for_interface_type<'db>(
    db: &'db dyn Database,
    struct_ast: &ast::ItemStruct<'db>,
    generics: &GenericParamsInfo<'db>,
    configs: &[StorageMemberConfig],
    metadata: &MacroPluginMetadata<'_>,
    storage_node_type: StorageInterfaceType,
    builder: &mut PatchBuilder<'db>,
) {
    let [args, params] = if generics.full_params.is_empty() {
        [RewriteNode::empty(), RewriteNode::empty()]
    } else {
        [
            format!("<{}>", generics.param_names.join(", ")),
            format!("<{}>", generics.full_params.join(", ")),
        ]
        .map(RewriteNode::Text)
    };
    let storage_node_info =
        StorageInterfaceInfo { db, node_type: storage_node_type.clone(), is_mutable: false };
    // Create Info with type and false for is_mutable
    add_interface_struct_definition(
        db,
        builder,
        struct_ast,
        params.clone(),
        &storage_node_info,
        metadata,
    );
    add_interface_impl(
        db,
        builder,
        struct_ast,
        (args.clone(), params.clone()),
        configs,
        &storage_node_info,
    );
    let mutable_storage_node_info =
        StorageInterfaceInfo { db, node_type: storage_node_type, is_mutable: true };
    // Create Info with type and true for is_mutable
    add_interface_struct_definition(
        db,
        builder,
        struct_ast,
        params.clone(),
        &mutable_storage_node_info,
        metadata,
    );
    add_interface_impl(
        db,
        builder,
        struct_ast,
        (args, params),
        configs,
        &mutable_storage_node_info,
    );
}

/// Adds the storage interface structs (two variants, mutable and immutable) and their constructor
/// impls to the given builder. This function is called from several places:
///  - From this plugin for adding storage nodes, and storage base trait.
///  - From the derive plugin of the `Store` trait which also generates a sub-pointers interface.
///  - From the contract storage plugin, which generates storage base trait.
pub fn handle_storage_interface_struct<'db>(
    db: &'db dyn Database,
    struct_ast: &ast::ItemStruct<'db>,
    configs: &[StorageMemberConfig],
    metadata: &MacroPluginMetadata<'_>,
) -> PatchBuilder<'db> {
    // Run for both StorageNode and StorageTrait
    let (origin, storage_interface_types) =
        if let Some(attr) = struct_ast.find_attr(db, STORAGE_NODE_ATTR) {
            (
                attr.as_syntax_node(),
                vec![StorageInterfaceType::StorageTrait, StorageInterfaceType::StorageNode],
            )
        } else if let Some(attr) = struct_ast.find_attr(db, STORAGE_ATTR) {
            (attr.as_syntax_node(), vec![StorageInterfaceType::StorageTrait])
        } else if let Some(arg) = has_derive(struct_ast, db, STORE_TRAIT) {
            (arg.as_syntax_node(), vec![StorageInterfaceType::StructSubPointers])
        } else {
            panic!("Invalid storage interface type.");
        };
    let mut builder = PatchBuilder::new_ex(db, &origin);
    let generics = GenericParamsInfo::new(db, struct_ast.generic_params(db));
    for interface_type in storage_interface_types {
        handle_storage_interface_for_interface_type(
            db,
            struct_ast,
            &generics,
            configs,
            metadata,
            interface_type,
            &mut builder,
        );
    }
    builder
}

/// Adds the storage interface enum and its constructor impl, for enums with sub-pointers.
pub fn handle_storage_interface_enum<'db>(
    db: &'db dyn Database,
    enum_ast: &ast::ItemEnum<'db>,
) -> (String, Vec<CodeMapping>) {
    let mut builder = PatchBuilder::new(db, enum_ast);
    let generics = GenericParamsInfo::new(db, enum_ast.generic_params(db));
    let [args, params] = if generics.full_params.is_empty() {
        [RewriteNode::empty(), RewriteNode::empty()]
    } else {
        [
            format!("<{}>", generics.param_names.join(", ")),
            format!("<{}>", generics.full_params.join(", ")),
        ]
        .map(RewriteNode::Text)
    };
    add_node_enum_definition(db, &mut builder, enum_ast, args.clone(), false);
    add_node_enum_impl(db, &mut builder, enum_ast, (args.clone(), params.clone()), false);
    add_node_enum_definition(db, &mut builder, enum_ast, args.clone(), true);
    add_node_enum_impl(db, &mut builder, enum_ast, (args, params), true);

    builder.build()
}

/// Generates the struct definition for the storage interface.
fn add_interface_struct_definition<'db>(
    db: &'db dyn Database,
    builder: &mut PatchBuilder<'db>,
    struct_ast: &ast::ItemStruct<'db>,
    params: RewriteNode<'db>,
    storage_node_info: &StorageInterfaceInfo<'db>,
    metadata: &MacroPluginMetadata<'_>,
) {
    let node_type_name = storage_node_info.node_type_name();
    let struct_name_rewrite_node = RewriteNode::from_ast_trimmed(&struct_ast.name(db));
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
            #[doc(hidden)]
            $struct_visibility$struct {node_type_name}$generic_params$ {{
            ",
        ),
        &[
            ("struct_visibility".to_string(), struct_visibility),
            ("object_name".to_string(), struct_name_rewrite_node),
            ("generic_params".to_string(), params),
        ]
        .into(),
    ));

    for field in struct_ast.members(db).elements(db) {
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
            &format!(
                "$attributes$    $field_visibility$$field_name$: {concrete_node_members_type},\n",
            ),
            &[
                ("attributes".to_string(), RewriteNode::from_ast(&field.attributes(db))),
                ("field_visibility".to_string(), field_visibility),
                ("field_name".to_string(), RewriteNode::from_ast_trimmed(&field.name(db))),
                (
                    "field_type".to_string(),
                    RewriteNode::from_ast_trimmed(&field.type_clause(db).ty(db)),
                ),
            ]
            .into(),
        ));
    }
    builder.add_str("}\n");
}

/// Generates the impl for the storage interface.
fn add_interface_impl<'db>(
    db: &'db dyn Database,
    builder: &mut PatchBuilder<'db>,
    struct_ast: &ast::ItemStruct<'db>,
    (args, params): (RewriteNode<'db>, RewriteNode<'db>),
    configs: &[StorageMemberConfig],
    storage_node_info: &StorageInterfaceInfo<'db>,
) {
    let struct_name = RewriteNode::from_ast_trimmed(&struct_ast.name(db));
    let node_type_name = storage_node_info.node_type_name();
    let node_impl_name = storage_node_info.node_impl_name();
    let node_trait_name = storage_node_info.node_trait_name();
    let node_type = storage_node_info.node_type();
    let node_init_function_name = storage_node_info.node_init_function_name();
    let originating_type = storage_node_info.originating_type();
    let node_constructor_prefix_code = storage_node_info.node_constructor_prefix_code();

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "#[doc(hidden)]
             impl {node_impl_name}$generic_params$ of {node_trait_name} {{
                 type {node_type} = {node_type_name}$generic_args$;
                 fn {node_init_function_name}(self: {originating_type}) -> \
             {node_type_name}$generic_args$ {{{node_constructor_prefix_code}
",
        ),
        &[
            ("object_name".to_string(), struct_name.clone()),
            ("generic_params".to_string(), params),
            ("generic_args".to_string(), args),
        ]
        .into(),
    ));

    let members = struct_ast.members(db);
    let fields = members.elements(db);
    let mut fields_iter = zip_eq(fields, configs).peekable();
    while let Some((field, config)) = fields_iter.next() {
        let field_name = RewriteNode::from_ast_trimmed(&field.name(db));
        let field_type = RewriteNode::from_ast_trimmed(&field.type_clause(db).ty(db));
        let field_selector_name =
            config.rename.as_deref().map_or_else(|| field_name.clone(), RewriteNode::text);
        let is_last = fields_iter.peek().is_none();
        builder.add_modified(RewriteNode::interpolate_patched(
            &storage_node_info.node_constructor_field_init_code(is_last, &field),
            &[
                ("field_selector_name".to_string(), field_selector_name),
                ("field_name".to_string(), field_name),
                ("field_type".to_string(), field_type),
            ]
            .into(),
        ));
    }

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!("        {} {{\n", storage_node_info.node_type_name()),
        &[("object_name".to_string(), struct_name)].into(),
    ));

    for field in struct_ast.members(db).elements(db) {
        builder.add_modified(RewriteNode::interpolate_patched(
            "           $field_name$: __$field_name$_value__,\n",
            &[("field_name".to_string(), RewriteNode::from_ast_trimmed(&field.name(db)))].into(),
        ));
    }

    builder.add_str("        }\n    }\n}\n");
}

/// Generates the enum definition for an enum with sub pointers.
fn add_node_enum_definition<'db>(
    db: &'db dyn Database,
    builder: &mut PatchBuilder<'db>,
    enum_ast: &ast::ItemEnum<'db>,
    args: RewriteNode<'db>,
    is_mutable: bool,
) {
    let storage_node_info = StorageInterfaceInfo::from_enum_ast(db, enum_ast, is_mutable).unwrap();
    let node_type_name = storage_node_info.node_type_name();

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "#[derive(Drop, Copy)]
            enum {node_type_name}$generic_args$ {{
                "
        ),
        &[("generic_args".to_string(), args)].into(),
    ));
    for variant in enum_ast.variants(db).elements(db) {
        let concrete_node_members_type = storage_node_info.concrete_node_members_type(&variant);
        let field_type = match variant.type_clause(db) {
            ast::OptionTypeClause::Empty(_) => RewriteNode::text("()"),
            ast::OptionTypeClause::TypeClause(tc) => RewriteNode::from_ast_trimmed(&tc.ty(db)),
        };

        builder.add_modified(RewriteNode::interpolate_patched(
            &format!("$attributes$    $field_name$: {concrete_node_members_type},\n",),
            &[
                ("attributes".to_string(), RewriteNode::from_ast(&variant.attributes(db))),
                ("field_name".to_string(), RewriteNode::from_ast_trimmed(&variant.name(db))),
                ("field_type".to_string(), field_type),
            ]
            .into(),
        ));
    }
    builder.add_str("}\n");
}

/// Generates the impl for the storage node for an enum with sub pointers.
fn add_node_enum_impl<'db>(
    db: &'db dyn Database,
    builder: &mut PatchBuilder<'db>,
    enum_ast: &ast::ItemEnum<'db>,
    (args, params): (RewriteNode<'db>, RewriteNode<'db>),
    is_mutable: bool,
) {
    let storage_node_info = StorageInterfaceInfo::from_enum_ast(db, enum_ast, is_mutable).unwrap();
    let enum_name = RewriteNode::from_ast_trimmed(&enum_ast.name(db));
    let node_type_name = storage_node_info.node_type_name();
    let node_impl_name = storage_node_info.node_impl_name();
    let node_trait_name = storage_node_info.node_trait_name();
    let node_type = storage_node_info.node_type();
    let node_init_function_name = storage_node_info.node_init_function_name();
    let originating_type = storage_node_info.originating_type();
    let node_constructor_prefix_code = storage_node_info.node_constructor_prefix_code();

    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!{"
            impl {node_impl_name}$generic_params$ of {node_trait_name} {{
                type {node_type} = {node_type_name}$generic_args$;
                fn {node_init_function_name}(self: {originating_type}) -> {node_type_name}$generic_args$ {{
                    {node_constructor_prefix_code}
        ",},
        &[
            ("object_name".to_string(), enum_name.clone()),
            ("generic_params".to_string(), params),
            ("generic_args".to_string(), args),
        ]
        .into(),
    ));

    let mut default_index = None;
    for (index, variant) in enum_ast.variants(db).elements(db).enumerate() {
        let variant_selector = if variant.attributes(db).has_attr(db, "default") {
            // If there is more than one default variant, a diagnostic is already emitted from
            // derive(Store).
            // TODO(Gil): Consider adding a diagnostic here as well.
            default_index = Some(index);
            0
        } else {
            index + usize::from(default_index.is_none())
        };

        builder.add_modified(RewriteNode::interpolate_patched(
            &storage_node_info.node_constructor_field_init_code(false, &variant),
            &[
                ("object_name".to_string(), enum_name.clone()),
                ("field_name".to_string(), RewriteNode::from_ast_trimmed(&variant.name(db))),
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

/// The configuration of a storage struct member.
#[derive(Debug)]
pub struct StorageMemberConfig {
    pub kind: StorageMemberKind,
    pub rename: Option<String>,
}

/// The kind of a storage struct member.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StorageMemberKind {
    /// A basic storage member - would be stored in a separate storage slot.
    Basic,
    /// A flat storage member - would be stored in the same storage slot as the struct.
    Flat,
    /// A sub-storage member - would be stored in the same storage slot, but used for components.
    SubStorage,
}

/// Gets the storage configuration for members of a struct.
pub fn struct_members_storage_configs<'db>(
    db: &'db dyn Database,
    struct_ast: &ast::ItemStruct<'db>,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) -> Vec<StorageMemberConfig> {
    struct_ast
        .members(db)
        .elements(db)
        .map(|member| get_member_storage_config(db, &member, diagnostics))
        .collect()
}

/// Gets the storage configuration of a struct member.
pub fn get_member_storage_config<'db>(
    db: &'db dyn Database,
    member: &ast::Member<'db>,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) -> StorageMemberConfig {
    let mut result = StorageMemberConfig { kind: StorageMemberKind::Basic, rename: None };
    for attr in member.query_attr(db, FLAT_ATTR) {
        let attr = attr.structurize(db);
        if result.kind != StorageMemberKind::Basic {
            diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr,
                "Multiple storage attributes are not allowed.".to_string(),
            ));
        }
        if !attr.args.is_empty() {
            diagnostics.push(PluginDiagnostic::error(
                attr.args_stable_ptr,
                "Unexpected arguments.".to_string(),
            ));
        }
        result.kind = StorageMemberKind::Flat;
    }
    for attr in member.query_attr(db, SUBSTORAGE_ATTR) {
        if result.kind != StorageMemberKind::Basic {
            diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(db),
                "Multiple storage attributes are not allowed.".to_string(),
            ));
        }
        validate_v0(db, diagnostics, &attr, SUBSTORAGE_ATTR);
        result.kind = StorageMemberKind::SubStorage;
    }
    for attr in member.query_attr(db, RENAME_ATTR) {
        if result.kind != StorageMemberKind::Basic {
            diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(db),
                "The `rename` attribute cannot be used with other storage attributes.".to_string(),
            ));
        }
        if result.rename.is_some() {
            diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(db),
                "Multiple `rename` attributes are not allowed.".to_string(),
            ));
        }
        let attr = attr.structurize(db);
        let [
            AttributeArg {
                variant: AttributeArgVariant::Unnamed(ast::Expr::String(value)), ..
            },
        ] = &attr.args[..]
        else {
            diagnostics.push(PluginDiagnostic::error(
                attr.args_stable_ptr,
                "Unexpected arguments, expected single string argument.".to_string(),
            ));
            continue;
        };
        result.rename = value.string_value(db);
    }
    result
}
