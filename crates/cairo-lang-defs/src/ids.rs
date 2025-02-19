// The following ids represent all the definitions in the code.
// Roughly, this refers to the first appearance of each identifier.
// Everything that can be returned by "Go to definition" is a definition.
//
// Examples:
// * let x = 5.
// Has a definition for the variable "x".
// * fn foo<T>(a: T){ return (); }.
// Has 3 definitions:
//   * Function "foo".
//   * Generic parameter "T" (only the first occurrence of "T").
//   * Function parameter "a".
// * trait MyTrait{ fn foo() -> (); }
// Has 2 definitions:
//   * Trait "MyTrait"
//   * TraitFunction "foo".
// * impl A for MyTrait{ fn foo() -> (){...} }
// Has 2 definitions:
//   * Impl "A"
//   * ImplFunction "foo".
//
// Call sites, variable usages, assignments, etc. are NOT definitions.

use std::hash::{Hash, Hasher};
use std::sync::Arc;

use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_diagnostics::Maybe;
pub use cairo_lang_filesystem::ids::UnstableSalsaId;
use cairo_lang_filesystem::ids::{CrateId, FileId};
use cairo_lang_syntax::node::ast::TerminalIdentifierGreen;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, HasName, NameGreen};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::stable_ptr::SyntaxStablePtr;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::{Intern, LookupIntern, OptionFrom, define_short_id, require};
use smol_str::SmolStr;

use crate::db::DefsGroup;
use crate::diagnostic_utils::StableLocation;
use crate::plugin::{InlineMacroExprPlugin, MacroPlugin};

// A trait for an id for a language element.
pub trait LanguageElementId {
    fn module_file_id(&self, db: &dyn DefsGroup) -> ModuleFileId;
    fn untyped_stable_ptr(&self, db: &dyn DefsGroup) -> SyntaxStablePtrId;

    fn parent_module(&self, db: &dyn DefsGroup) -> ModuleId {
        self.module_file_id(db).0
    }
    fn file_index(&self, db: &dyn DefsGroup) -> FileIndex {
        self.module_file_id(db).1
    }

    fn stable_location(&self, db: &dyn DefsGroup) -> StableLocation;
}

pub trait NamedLanguageElementLongId {
    fn name(&self, db: &dyn DefsGroup) -> SmolStr;
    fn name_identifier(&self, db: &dyn DefsGroup) -> ast::TerminalIdentifier;
}
pub trait NamedLanguageElementId: LanguageElementId {
    fn name(&self, db: &dyn DefsGroup) -> SmolStr;
    fn name_identifier(&self, db: &dyn DefsGroup) -> ast::TerminalIdentifier;
}
pub trait TopLevelLanguageElementId: NamedLanguageElementId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.parent_module(db).full_path(db), self.name(db))
    }
}

/// Utility macro for defining an id for a top level language element.
/// 1. Defines a long id representing some element by a module_id and a stable pointer.
/// 2. Defines a short id to be used for interning of the long id.
/// 3. Requires the lookup function name for the lookup of the long id from the short id, as defined
///    in DefsGroup.
/// 4. Implements `NamedLanguageElementId` using a key_field. See the documentation of
///    'define_short_id' and `stable_ptr.rs` for more details.
macro_rules! define_top_level_language_element_id {
    ($short_id:ident, $long_id:ident, $ast_ty:ty, $lookup:ident, $intern:ident) => {
        define_named_language_element_id!($short_id, $long_id, $ast_ty, $lookup, $intern);
        impl TopLevelLanguageElementId for $short_id {}
    };
}

/// Utility macro for defining an id for a language element, with a name but without full path.
/// This is used by `define_top_level_language_element_id` (see its documentation), but doesn't
/// implement TopLevelLanguageElementId for the type.
///
/// Note: prefer to use `define_top_level_language_element_id`, unless you need to overwrite the
/// behavior of `TopLevelLanguageElementId` for the type.
macro_rules! define_named_language_element_id {
    ($short_id:ident, $long_id:ident, $ast_ty:ty, $lookup:ident, $intern:ident) => {
        define_language_element_id_basic!($short_id, $long_id, $ast_ty, $lookup, $intern);
        impl<'a, T: ?Sized + cairo_lang_utils::Upcast<dyn DefsGroup + 'a>>
            cairo_lang_debug::DebugWithDb<T> for $long_id
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &T) -> std::fmt::Result {
                let db: &(dyn DefsGroup + 'a) = db.upcast();
                let $long_id(module_file_id, _stable_ptr) = self;
                write!(
                    f,
                    "{}({}::{})",
                    stringify!($short_id),
                    module_file_id.0.full_path(db),
                    self.name(db)
                )
            }
        }
        impl NamedLanguageElementLongId for $long_id {
            fn name(&self, db: &dyn DefsGroup) -> SmolStr {
                let syntax_db = db.upcast();
                let terminal_green = self.1.name_green(syntax_db);
                terminal_green.identifier(syntax_db)
            }
            fn name_identifier(&self, db: &dyn DefsGroup) -> ast::TerminalIdentifier {
                let syntax_db = db.upcast();
                self.1.lookup(syntax_db).name(syntax_db)
            }
        }
        impl NamedLanguageElementId for $short_id {
            fn name(&self, db: &dyn DefsGroup) -> SmolStr {
                db.$lookup(*self).name(db)
            }
            fn name_identifier(&self, db: &dyn DefsGroup) -> ast::TerminalIdentifier {
                db.$lookup(*self).name_identifier(db)
            }
        }
    };
}

/// Utility macro for defining an id for a language element, without a name and full path.
/// This is used by `define_named_language_element_id` (see its documentation), but doesn't
/// implement NamedLanguageElementId for the type.
///
/// Use for language elements that are not top level and don't have a name.
macro_rules! define_language_element_id_basic {
    ($short_id:ident, $long_id:ident, $ast_ty:ty, $lookup:ident, $intern:ident) => {
        #[derive(Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $long_id(pub ModuleFileId, pub <$ast_ty as TypedSyntaxNode>::StablePtr);
        define_short_id!($short_id, $long_id, DefsGroup, $lookup, $intern);
        impl $short_id {
            pub fn stable_ptr(
                &self,
                db: &dyn DefsGroup,
            ) -> <$ast_ty as TypedSyntaxNode>::StablePtr {
                db.$lookup(*self).1
            }
        }
        impl LanguageElementId for $short_id {
            fn module_file_id(&self, db: &dyn DefsGroup) -> ModuleFileId {
                db.$lookup(*self).0
            }
            fn untyped_stable_ptr(&self, db: &dyn DefsGroup) -> SyntaxStablePtrId {
                self.stable_ptr(db).untyped()
            }
            fn stable_location(&self, db: &dyn DefsGroup) -> StableLocation {
                let $long_id(_module_file_id, stable_ptr) = db.$lookup(*self);
                StableLocation::new(stable_ptr.untyped())
            }
        }
    };
}

/// Defines and implements LanguageElementId for a subset of other language elements.
macro_rules! define_language_element_id_as_enum {
    (
        #[toplevel]
        $(#[doc = $doc:expr])*
        pub enum $enum_name:ident {
            $($variant:ident ($variant_ty:ty),)*
        }
    ) => {
        toplevel_enum! {
            pub enum $enum_name {
                $($variant($variant_ty),)*
            }
        }
        define_language_element_id_as_enum! {
            $(#[doc = $doc])*
            pub enum $enum_name {
                $($variant($variant_ty),)*
            }
        }
    };
    (
        $(#[doc = $doc:expr])*
        pub enum $enum_name:ident {
            $($variant:ident ($variant_ty:ty),)*
        }
    ) => {
        $(#[doc = $doc])*
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        pub enum $enum_name {
            $($variant($variant_ty),)*
        }
        impl<T: ?Sized + cairo_lang_utils::Upcast<dyn DefsGroup + 'static>> cairo_lang_debug::DebugWithDb<T>
            for $enum_name
        {
            fn fmt(
                &self,
                f: &mut std::fmt::Formatter<'_>,
                db: &T,
            ) -> std::fmt::Result {
                let db : &(dyn DefsGroup + 'static) = db.upcast();
                match self {
                    $(
                        $enum_name::$variant(id) => id.fmt(f, db),
                    )*
                }
            }
        }
        impl LanguageElementId for $enum_name {
            fn module_file_id(&self, db: &dyn DefsGroup) -> ModuleFileId {
                match self {
                    $(
                        $enum_name::$variant(id) => id.module_file_id(db),
                    )*
                }
            }
            fn untyped_stable_ptr(&self, db: &dyn DefsGroup) -> SyntaxStablePtrId {
                match self {
                    $(
                        $enum_name::$variant(id) => id.untyped_stable_ptr(db),
                    )*
                }
            }
            fn stable_location(&self, db: &dyn DefsGroup) -> StableLocation {
                 match self {
                    $(
                        $enum_name::$variant(id) => id.stable_location(db),
                    )*
                }
            }

        }

        // Conversion from enum to its child.
        $(
            impl OptionFrom<$enum_name> for $variant_ty {
                fn option_from(other: $enum_name) -> Option<Self> {
                    #[allow(irrefutable_let_patterns)]
                    if let $enum_name::$variant(id) = other {
                        Some(id)
                    } else {
                        None
                    }
                }
            }
        )*
    }
}

macro_rules! toplevel_enum {
    (
        pub enum $enum_name:ident {
            $($variant:ident ($variant_ty:ty),)*
        }
    ) => {
        impl NamedLanguageElementId for $enum_name {
            fn name(&self, db: &dyn DefsGroup) -> SmolStr {
                match self {
                    $(
                        $enum_name::$variant(id) => id.name(db),
                    )*
                }
            }
            fn name_identifier(&self, db: &dyn DefsGroup) -> ast::TerminalIdentifier {
                match self {
                    $(
                        $enum_name::$variant(id) => id.name_identifier(db),
                    )*
                }
            }
        }
        impl TopLevelLanguageElementId for $enum_name {}
    }
}

/// Id for a module. Either the root module of a crate, or a submodule.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModuleId {
    CrateRoot(CrateId),
    Submodule(SubmoduleId),
}
impl ModuleId {
    pub fn full_path(&self, db: &dyn DefsGroup) -> String {
        match self {
            ModuleId::CrateRoot(id) => id.lookup_intern(db).name().into(),
            ModuleId::Submodule(id) => {
                format!("{}::{}", id.parent_module(db).full_path(db), id.name(db))
            }
        }
    }
    pub fn name(&self, db: &dyn DefsGroup) -> SmolStr {
        match self {
            ModuleId::CrateRoot(id) => id.lookup_intern(db).name(),
            ModuleId::Submodule(id) => id.name(db),
        }
    }
    pub fn owning_crate(&self, db: &dyn DefsGroup) -> CrateId {
        match self {
            ModuleId::CrateRoot(crate_id) => *crate_id,
            ModuleId::Submodule(submodule) => submodule.parent_module(db).owning_crate(db),
        }
    }
}
impl DebugWithDb<dyn DefsGroup> for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn DefsGroup) -> std::fmt::Result {
        write!(f, "ModuleId({})", self.full_path(db))
    }
}
/// Index of file in module.
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct FileIndex(pub usize);
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct ModuleFileId(pub ModuleId, pub FileIndex);
impl ModuleFileId {
    pub fn file_id(&self, db: &dyn DefsGroup) -> Maybe<FileId> {
        Ok(db.module_files(self.0)?[self.1.0])
    }
}

/// An id for a file defined out of the filesystem crate, for files generated by plugins.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PluginGeneratedFileLongId {
    /// The module that the file was generated from.
    pub module_id: ModuleId,
    /// The stable pointer the file was generated from being ran on.
    pub stable_ptr: SyntaxStablePtrId,
    /// The name of the generated file to differentiate between different generated files.
    pub name: SmolStr,
}
define_short_id!(
    PluginGeneratedFileId,
    PluginGeneratedFileLongId,
    DefsGroup,
    lookup_intern_plugin_generated_file,
    intern_plugin_generated_file
);

/// An ID allowing for interning the [`MacroPlugin`] into Salsa database.
#[derive(Clone, Debug)]
pub struct MacroPluginLongId(pub Arc<dyn MacroPlugin>);

impl MacroPlugin for MacroPluginLongId {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        metadata: &crate::plugin::MacroPluginMetadata<'_>,
    ) -> crate::plugin::PluginResult {
        self.0.generate_code(db, item_ast, metadata)
    }

    fn declared_attributes(&self) -> Vec<String> {
        self.0.declared_attributes()
    }

    fn declared_derives(&self) -> Vec<String> {
        self.0.declared_derives()
    }

    fn executable_attributes(&self) -> Vec<String> {
        self.0.executable_attributes()
    }

    fn phantom_type_attributes(&self) -> Vec<String> {
        self.0.phantom_type_attributes()
    }
}

// `PartialEq` and `Hash` cannot be derived on `Arc<dyn ...>`,
// but pointer-based equality and hash semantics are enough in this case.
impl PartialEq for MacroPluginLongId {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for MacroPluginLongId {}

impl Hash for MacroPluginLongId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state)
    }
}

define_short_id!(
    MacroPluginId,
    MacroPluginLongId,
    DefsGroup,
    lookup_intern_macro_plugin,
    intern_macro_plugin
);

/// An ID allowing for interning the [`InlineMacroExprPlugin`] into Salsa database.
#[derive(Clone, Debug)]
pub struct InlineMacroExprPluginLongId(pub Arc<dyn InlineMacroExprPlugin>);

impl InlineMacroExprPlugin for InlineMacroExprPluginLongId {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: &ast::ExprInlineMacro,
        metadata: &crate::plugin::MacroPluginMetadata<'_>,
    ) -> crate::plugin::InlinePluginResult {
        self.0.generate_code(db, item_ast, metadata)
    }

    fn documentation(&self) -> Option<String> {
        self.0.documentation()
    }
}

// `PartialEq` and `Hash` cannot be derived on `Arc<dyn ...>`,
// but pointer-based equality and hash semantics are enough in this case.
impl PartialEq for InlineMacroExprPluginLongId {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for InlineMacroExprPluginLongId {}

impl Hash for InlineMacroExprPluginLongId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state)
    }
}

define_short_id!(
    InlineMacroExprPluginId,
    InlineMacroExprPluginLongId,
    DefsGroup,
    lookup_intern_inline_macro_plugin,
    intern_inline_macro_plugin
);

define_language_element_id_as_enum! {
    #[toplevel]
    /// Id for direct children of a module.
    pub enum ModuleItemId {
        Constant(ConstantId),
        Submodule(SubmoduleId),
        Use(UseId),
        FreeFunction(FreeFunctionId),
        Struct(StructId),
        Enum(EnumId),
        TypeAlias(ModuleTypeAliasId),
        ImplAlias(ImplAliasId),
        Trait(TraitId),
        Impl(ImplDefId),
        ExternType(ExternTypeId),
        ExternFunction(ExternFunctionId),
    }
}
define_top_level_language_element_id!(
    SubmoduleId,
    SubmoduleLongId,
    ast::ItemModule,
    lookup_intern_submodule,
    intern_submodule
);
impl UnstableSalsaId for SubmoduleId {
    fn get_internal_id(&self) -> &salsa::InternId {
        &self.0
    }
}

define_top_level_language_element_id!(
    ConstantId,
    ConstantLongId,
    ast::ItemConstant,
    lookup_intern_constant,
    intern_constant
);
define_language_element_id_basic!(
    GlobalUseId,
    GlobalUseLongId,
    ast::UsePathStar,
    lookup_intern_global_use,
    intern_global_use
);
define_top_level_language_element_id!(
    UseId,
    UseLongId,
    ast::UsePathLeaf,
    lookup_intern_use,
    intern_use
);
define_top_level_language_element_id!(
    FreeFunctionId,
    FreeFunctionLongId,
    ast::FunctionWithBody,
    lookup_intern_free_function,
    intern_free_function
);

impl UnstableSalsaId for FreeFunctionId {
    fn get_internal_id(&self) -> &salsa::InternId {
        &self.0
    }
}

// --- Impls ---
define_top_level_language_element_id!(
    ImplDefId,
    ImplDefLongId,
    ast::ItemImpl,
    lookup_intern_impl_def,
    intern_impl_def
);

// --- Impl type items ---
define_named_language_element_id!(
    ImplTypeDefId,
    ImplTypeDefLongId,
    ast::ItemTypeAlias,
    lookup_intern_impl_type_def,
    intern_impl_type_def
);
impl ImplTypeDefId {
    pub fn impl_def_id(&self, db: &dyn DefsGroup) -> ImplDefId {
        let ImplTypeDefLongId(module_file_id, ptr) = self.lookup_intern(db);

        // Impl type ast lies 3 levels below the impl ast.
        let impl_ptr = ast::ItemImplPtr(ptr.untyped().nth_parent(db.upcast(), 3));
        ImplDefLongId(module_file_id, impl_ptr).intern(db)
    }
}
impl TopLevelLanguageElementId for ImplTypeDefId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.impl_def_id(db).full_path(db), self.name(db))
    }
}

// --- Impl constant items ---
define_named_language_element_id!(
    ImplConstantDefId,
    ImplConstantDefLongId,
    ast::ItemConstant,
    lookup_intern_impl_constant_def,
    intern_impl_constant_def
);
impl ImplConstantDefId {
    pub fn impl_def_id(&self, db: &dyn DefsGroup) -> ImplDefId {
        let ImplConstantDefLongId(module_file_id, ptr) = self.lookup_intern(db);

        // Impl constant ast lies 3 levels below the impl ast.
        let impl_ptr = ast::ItemImplPtr(ptr.untyped().nth_parent(db.upcast(), 3));
        ImplDefLongId(module_file_id, impl_ptr).intern(db)
    }
}
impl TopLevelLanguageElementId for ImplConstantDefId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.impl_def_id(db).full_path(db), self.name(db))
    }
}

// --- Impl Impl items ---
define_named_language_element_id!(
    ImplImplDefId,
    ImplImplDefLongId,
    ast::ItemImplAlias,
    lookup_intern_impl_impl_def,
    intern_impl_impl_def
);
impl ImplImplDefId {
    pub fn impl_def_id(&self, db: &dyn DefsGroup) -> ImplDefId {
        let ImplImplDefLongId(module_file_id, ptr) = self.lookup_intern(db);

        // Impl constant ast lies 3 levels below the impl ast.
        let impl_ptr = ast::ItemImplPtr(ptr.untyped().nth_parent(db.upcast(), 3));
        ImplDefLongId(module_file_id, impl_ptr).intern(db)
    }
}
impl TopLevelLanguageElementId for ImplImplDefId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.impl_def_id(db).full_path(db), self.name(db))
    }
}

// --- Impl functions ---
define_named_language_element_id!(
    ImplFunctionId,
    ImplFunctionLongId,
    ast::FunctionWithBody,
    lookup_intern_impl_function,
    intern_impl_function
);
impl ImplFunctionId {
    pub fn impl_def_id(&self, db: &dyn DefsGroup) -> ImplDefId {
        let ImplFunctionLongId(module_file_id, ptr) = self.lookup_intern(db);

        // Impl function ast lies 3 levels below the impl ast.
        let impl_ptr = ast::ItemImplPtr(ptr.untyped().nth_parent(db.upcast(), 3));
        ImplDefLongId(module_file_id, impl_ptr).intern(db)
    }
}
impl UnstableSalsaId for ImplFunctionId {
    fn get_internal_id(&self) -> &salsa::InternId {
        &self.0
    }
}
impl TopLevelLanguageElementId for ImplFunctionId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.impl_def_id(db).full_path(db), self.name(db))
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// Represents a function that has a body.
    pub enum FunctionWithBodyId {
        Free(FreeFunctionId),
        Impl(ImplFunctionId),
        Trait(TraitFunctionId),
    }
}

define_top_level_language_element_id!(
    ExternFunctionId,
    ExternFunctionLongId,
    ast::ItemExternFunction,
    lookup_intern_extern_function,
    intern_extern_function
);
define_top_level_language_element_id!(
    StructId,
    StructLongId,
    ast::ItemStruct,
    lookup_intern_struct,
    intern_struct
);
define_top_level_language_element_id!(
    EnumId,
    EnumLongId,
    ast::ItemEnum,
    lookup_intern_enum,
    intern_enum
);
define_top_level_language_element_id!(
    ModuleTypeAliasId,
    ModuleTypeAliasLongId,
    ast::ItemTypeAlias,
    lookup_intern_module_type_alias,
    intern_module_type_alias
);
define_top_level_language_element_id!(
    ImplAliasId,
    ImplAliasLongId,
    ast::ItemImplAlias,
    lookup_intern_impl_alias,
    intern_impl_alias
);
define_top_level_language_element_id!(
    ExternTypeId,
    ExternTypeLongId,
    ast::ItemExternType,
    lookup_intern_extern_type,
    intern_extern_type
);

// --- Trait ---
define_top_level_language_element_id!(
    TraitId,
    TraitLongId,
    ast::ItemTrait,
    lookup_intern_trait,
    intern_trait
);

// --- Trait type items ---
define_named_language_element_id!(
    TraitTypeId,
    TraitTypeLongId,
    ast::TraitItemType,
    lookup_intern_trait_type,
    intern_trait_type
);
impl TraitTypeId {
    pub fn trait_id(&self, db: &dyn DefsGroup) -> TraitId {
        let TraitTypeLongId(module_file_id, ptr) = self.lookup_intern(db);
        // Trait type ast lies 3 levels below the trait ast.
        let trait_ptr = ast::ItemTraitPtr(ptr.untyped().nth_parent(db.upcast(), 3));
        TraitLongId(module_file_id, trait_ptr).intern(db)
    }
}
impl TopLevelLanguageElementId for TraitTypeId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.trait_id(db).full_path(db), self.name(db))
    }
}
impl UnstableSalsaId for TraitTypeId {
    fn get_internal_id(&self) -> &salsa::InternId {
        &self.0
    }
}

// --- Trait constant items ---
define_named_language_element_id!(
    TraitConstantId,
    TraitConstantLongId,
    ast::TraitItemConstant,
    lookup_intern_trait_constant,
    intern_trait_constant
);
impl TraitConstantId {
    pub fn trait_id(&self, db: &dyn DefsGroup) -> TraitId {
        let TraitConstantLongId(module_file_id, ptr) = self.lookup_intern(db);
        // Trait constant ast lies 3 levels below the trait ast.
        let trait_ptr = ast::ItemTraitPtr(ptr.untyped().nth_parent(db.upcast(), 3));
        TraitLongId(module_file_id, trait_ptr).intern(db)
    }
}
impl TopLevelLanguageElementId for TraitConstantId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.trait_id(db).full_path(db), self.name(db))
    }
}

// --- Trait impl items ---
define_named_language_element_id!(
    TraitImplId,
    TraitImplLongId,
    ast::TraitItemImpl,
    lookup_intern_trait_impl,
    intern_trait_impl
);
impl TraitImplId {
    pub fn trait_id(&self, db: &dyn DefsGroup) -> TraitId {
        let TraitImplLongId(module_file_id, ptr) = self.lookup_intern(db);
        // Trait impl ast lies 3 levels below the trait ast.
        let trait_ptr = ast::ItemTraitPtr(ptr.untyped().nth_parent(db.upcast(), 3));
        TraitLongId(module_file_id, trait_ptr).intern(db)
    }
}
impl TopLevelLanguageElementId for TraitImplId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.trait_id(db).full_path(db), self.name(db))
    }
}

// --- Trait functions ---
define_named_language_element_id!(
    TraitFunctionId,
    TraitFunctionLongId,
    ast::TraitItemFunction,
    lookup_intern_trait_function,
    intern_trait_function
);
impl TraitFunctionId {
    pub fn trait_id(&self, db: &dyn DefsGroup) -> TraitId {
        let TraitFunctionLongId(module_file_id, ptr) = self.lookup_intern(db);
        // Trait function ast lies 3 levels below the trait ast.
        let trait_ptr = ast::ItemTraitPtr(ptr.untyped().nth_parent(db.upcast(), 3));
        TraitLongId(module_file_id, trait_ptr).intern(db)
    }
}
impl TopLevelLanguageElementId for TraitFunctionId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.trait_id(db).full_path(db), self.name(db))
    }
}

// --- Struct items ---
define_named_language_element_id!(
    MemberId,
    MemberLongId,
    ast::Member,
    lookup_intern_member,
    intern_member
);
impl MemberId {
    pub fn struct_id(&self, db: &dyn DefsGroup) -> StructId {
        let MemberLongId(module_file_id, ptr) = self.lookup_intern(db);
        let struct_ptr = ast::ItemStructPtr(ptr.untyped().nth_parent(db.upcast(), 2));
        StructLongId(module_file_id, struct_ptr).intern(db)
    }
}

impl TopLevelLanguageElementId for MemberId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.struct_id(db).full_path(db), self.name(db))
    }
}

// --- Enum variants ---
define_named_language_element_id!(
    VariantId,
    VariantLongId,
    ast::Variant,
    lookup_intern_variant,
    intern_variant
);
impl VariantId {
    pub fn enum_id(&self, db: &dyn DefsGroup) -> EnumId {
        let VariantLongId(module_file_id, ptr) = self.lookup_intern(db);
        let struct_ptr = ast::ItemEnumPtr(ptr.untyped().nth_parent(db.upcast(), 2));
        EnumLongId(module_file_id, struct_ptr).intern(db)
    }
}

impl TopLevelLanguageElementId for VariantId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.enum_id(db).full_path(db), self.name(db))
    }
}

define_language_element_id_as_enum! {
    /// Id for any variable definition.
    pub enum VarId {
        Param(ParamId),
        Local(LocalVarId),
        Item(StatementItemId),
        // TODO(spapini): Add var from pattern matching.
    }
}

// TODO(spapini): Override full_path for to include parents, for better debug.
define_top_level_language_element_id!(
    ParamId,
    ParamLongId,
    ast::Param,
    lookup_intern_param,
    intern_param
);
define_language_element_id_basic!(
    GenericParamId,
    GenericParamLongId,
    ast::GenericParam,
    lookup_intern_generic_param,
    intern_generic_param
);
impl GenericParamLongId {
    pub fn name(&self, db: &dyn SyntaxGroup) -> Option<SmolStr> {
        let SyntaxStablePtr::Child { key_fields, kind, .. } = self.1.0.lookup_intern(db) else {
            unreachable!()
        };
        require(!matches!(
            kind,
            SyntaxKind::GenericParamImplAnonymous | SyntaxKind::GenericParamNegativeImpl
        ))?;

        let name_green = TerminalIdentifierGreen(key_fields[0]);
        Some(name_green.identifier(db))
    }

    pub fn debug_name(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.name(db).unwrap_or_else(|| "_".into())
    }
    pub fn kind(&self, db: &dyn SyntaxGroup) -> GenericKind {
        let SyntaxStablePtr::Child { kind, .. } = self.1.0.lookup_intern(db) else {
            unreachable!()
        };
        match kind {
            SyntaxKind::GenericParamType => GenericKind::Type,
            SyntaxKind::GenericParamConst => GenericKind::Const,
            SyntaxKind::GenericParamImplNamed | SyntaxKind::GenericParamImplAnonymous => {
                GenericKind::Impl
            }
            SyntaxKind::GenericParamNegativeImpl => GenericKind::NegImpl,
            _ => unreachable!(),
        }
    }
    /// Retrieves the ID of the generic item holding this generic parameter.
    pub fn generic_item(&self, db: &dyn DefsGroup) -> GenericItemId {
        let item_ptr = self.1.0.nth_parent(db.upcast(), 3);
        GenericItemId::from_ptr(db, self.0, item_ptr)
    }
}
impl GenericParamId {
    pub fn name(&self, db: &dyn DefsGroup) -> Option<SmolStr> {
        self.lookup_intern(db).name(db.upcast())
    }
    pub fn debug_name(&self, db: &dyn DefsGroup) -> SmolStr {
        self.lookup_intern(db).debug_name(db.upcast())
    }
    pub fn format(&self, db: &dyn DefsGroup) -> String {
        let long_ids = self.lookup_intern(db);
        let SyntaxStablePtr::Child { key_fields, kind, .. } = long_ids.1.0.lookup_intern(db) else {
            unreachable!()
        };

        let syntax_db = db.upcast();
        if matches!(
            kind,
            SyntaxKind::GenericParamImplAnonymous | SyntaxKind::GenericParamNegativeImpl
        ) {
            // For anonymous impls print the declaration.
            return self.stable_location(db).syntax_node(db).get_text_without_trivia(syntax_db);
        }

        let name_green = TerminalIdentifierGreen(key_fields[0]);
        name_green.identifier(syntax_db).into()
    }

    pub fn kind(&self, db: &dyn DefsGroup) -> GenericKind {
        self.lookup_intern(db).kind(db.upcast())
    }
    pub fn generic_item(&self, db: &dyn DefsGroup) -> GenericItemId {
        self.lookup_intern(db).generic_item(db.upcast())
    }
}
impl DebugWithDb<dyn DefsGroup> for GenericParamLongId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn DefsGroup) -> std::fmt::Result {
        write!(
            f,
            "GenericParam{}({}::{})",
            self.kind(db.upcast()),
            self.generic_item(db).full_path(db),
            self.debug_name(db.upcast())
        )
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of a module item with generic parameters.
    pub enum GenericModuleItemId {
        FreeFunc(FreeFunctionId),
        ExternFunc(ExternFunctionId),
        TraitFunc(TraitFunctionId),
        ImplFunc(ImplFunctionId),
        Trait(TraitId),
        Impl(ImplDefId),
        Struct(StructId),
        Enum(EnumId),
        ExternType(ExternTypeId),
        TypeAlias(ModuleTypeAliasId),
        ImplAlias(ImplAliasId),
    }
}
define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of a trait item with generic parameters.
    pub enum GenericTraitItemId {
        Type(TraitTypeId),
    }
}
define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of a impl item with generic parameters.
    pub enum GenericImplItemId {
        Type(ImplTypeDefId),
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of an item with generic parameters.
    pub enum GenericItemId {
        ModuleItem(GenericModuleItemId),
        TraitItem(GenericTraitItemId),
        ImplItem(GenericImplItemId),
    }
}
impl GenericItemId {
    pub fn from_ptr(
        db: &dyn DefsGroup,
        module_file: ModuleFileId,
        stable_ptr: SyntaxStablePtrId,
    ) -> Self {
        let SyntaxStablePtr::Child { parent: parent0, kind, .. } = stable_ptr.lookup_intern(db)
        else {
            panic!()
        };
        match kind {
            SyntaxKind::FunctionDeclaration => {
                let SyntaxStablePtr::Child { parent: parent1, kind, .. } =
                    parent0.lookup_intern(db)
                else {
                    panic!()
                };
                match kind {
                    SyntaxKind::FunctionWithBody => {
                        // `FunctionWithBody` must be at least 2 levels below the root, and thus
                        // `parent1.parent()` is safe.
                        match parent1.parent(db.upcast()).lookup_intern(db) {
                            SyntaxStablePtr::Root(_, _) => {
                                GenericItemId::ModuleItem(GenericModuleItemId::FreeFunc(
                                    FreeFunctionLongId(
                                        module_file,
                                        ast::FunctionWithBodyPtr(parent0),
                                    )
                                    .intern(db),
                                ))
                            }
                            SyntaxStablePtr::Child { kind, .. } => match kind {
                                SyntaxKind::ModuleBody => {
                                    GenericItemId::ModuleItem(GenericModuleItemId::FreeFunc(
                                        FreeFunctionLongId(
                                            module_file,
                                            ast::FunctionWithBodyPtr(parent0),
                                        )
                                        .intern(db),
                                    ))
                                }
                                SyntaxKind::ImplBody => {
                                    GenericItemId::ModuleItem(GenericModuleItemId::ImplFunc(
                                        ImplFunctionLongId(
                                            module_file,
                                            ast::FunctionWithBodyPtr(parent0),
                                        )
                                        .intern(db),
                                    ))
                                }
                                _ => panic!(),
                            },
                        }
                    }
                    SyntaxKind::ItemExternFunction => {
                        GenericItemId::ModuleItem(GenericModuleItemId::ExternFunc(
                            ExternFunctionLongId(module_file, ast::ItemExternFunctionPtr(parent0))
                                .intern(db),
                        ))
                    }
                    SyntaxKind::TraitItemFunction => {
                        GenericItemId::ModuleItem(GenericModuleItemId::TraitFunc(
                            TraitFunctionLongId(module_file, ast::TraitItemFunctionPtr(parent0))
                                .intern(db),
                        ))
                    }
                    _ => panic!(),
                }
            }
            SyntaxKind::ItemImpl => GenericItemId::ModuleItem(GenericModuleItemId::Impl(
                ImplDefLongId(module_file, ast::ItemImplPtr(stable_ptr)).intern(db),
            )),
            SyntaxKind::ItemTrait => GenericItemId::ModuleItem(GenericModuleItemId::Trait(
                TraitLongId(module_file, ast::ItemTraitPtr(stable_ptr)).intern(db),
            )),
            SyntaxKind::ItemStruct => GenericItemId::ModuleItem(GenericModuleItemId::Struct(
                StructLongId(module_file, ast::ItemStructPtr(stable_ptr)).intern(db),
            )),
            SyntaxKind::ItemEnum => GenericItemId::ModuleItem(GenericModuleItemId::Enum(
                EnumLongId(module_file, ast::ItemEnumPtr(stable_ptr)).intern(db),
            )),
            SyntaxKind::ItemExternType => {
                GenericItemId::ModuleItem(GenericModuleItemId::ExternType(
                    ExternTypeLongId(module_file, ast::ItemExternTypePtr(stable_ptr)).intern(db),
                ))
            }
            SyntaxKind::ItemTypeAlias => {
                // `ItemTypeAlias` must be at least 2 levels below the root, and thus
                // `parent0.kind()` is safe.
                match parent0.kind(db.upcast()) {
                    SyntaxKind::ModuleItemList => {
                        GenericItemId::ModuleItem(GenericModuleItemId::TypeAlias(
                            ModuleTypeAliasLongId(module_file, ast::ItemTypeAliasPtr(stable_ptr))
                                .intern(db),
                        ))
                    }
                    SyntaxKind::ImplItemList => GenericItemId::ImplItem(GenericImplItemId::Type(
                        ImplTypeDefLongId(module_file, ast::ItemTypeAliasPtr(stable_ptr))
                            .intern(db),
                    )),
                    _ => panic!(),
                }
            }
            SyntaxKind::ItemImplAlias => GenericItemId::ModuleItem(GenericModuleItemId::ImplAlias(
                ImplAliasLongId(module_file, ast::ItemImplAliasPtr(stable_ptr)).intern(db),
            )),
            SyntaxKind::TraitItemType => GenericItemId::TraitItem(GenericTraitItemId::Type(
                TraitTypeLongId(module_file, ast::TraitItemTypePtr(stable_ptr)).intern(db),
            )),
            _ => panic!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum GenericKind {
    Type,
    Const,
    Impl,
    NegImpl,
}
impl std::fmt::Display for GenericKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericKind::Type => write!(f, "Type"),
            GenericKind::Const => write!(f, "Const"),
            GenericKind::Impl => write!(f, "Impl"),
            GenericKind::NegImpl => write!(f, "-Impl"),
        }
    }
}

// TODO(spapini): change this to a binding inside a pattern.
// TODO(spapini): Override full_path to include parents, for better debug.
define_language_element_id_basic!(
    LocalVarId,
    LocalVarLongId,
    ast::TerminalIdentifier,
    lookup_intern_local_var,
    intern_local_var
);
impl DebugWithDb<dyn DefsGroup> for LocalVarLongId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn DefsGroup) -> std::fmt::Result {
        let syntax_db = db.upcast();
        let LocalVarLongId(module_file_id, ptr) = self;
        let text = ptr.lookup(syntax_db).text(syntax_db);
        write!(f, "LocalVarId({}::{})", module_file_id.0.full_path(db), text)
    }
}

define_top_level_language_element_id!(
    StatementConstId,
    StatementConstLongId,
    ast::ItemConstant,
    lookup_intern_statement_const,
    intern_statement_const
);

define_top_level_language_element_id!(
    StatementUseId,
    StatementUseLongId,
    ast::UsePathLeaf,
    lookup_intern_statement_use,
    intern_statement_use
);

define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of a function's signature in the code.
    pub enum FunctionTitleId {
        Free(FreeFunctionId),
        Extern(ExternFunctionId),
        Trait(TraitFunctionId),
        Impl(ImplFunctionId),
    }
}
impl FunctionTitleId {
    pub fn format(&self, db: &dyn DefsGroup) -> String {
        let function_name = match *self {
            FunctionTitleId::Free(_) | FunctionTitleId::Extern(_) => self.name(db).into(),
            FunctionTitleId::Trait(id) => id.full_path(db),
            FunctionTitleId::Impl(id) => id.full_path(db),
        };
        format!("{}::{}", self.parent_module(db).full_path(db), function_name)
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// Generic type ids enum.
    pub enum GenericTypeId {
        Struct(StructId),
        Enum(EnumId),
        Extern(ExternTypeId),
        // TODO(spapini): associated types in impls.
    }
}
impl GenericTypeId {
    pub fn format(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.parent_module(db).full_path(db), self.name(db))
    }
}

/// Conversion from ModuleItemId to GenericTypeId.
impl OptionFrom<ModuleItemId> for GenericTypeId {
    fn option_from(item: ModuleItemId) -> Option<Self> {
        match item {
            ModuleItemId::Struct(id) => Some(GenericTypeId::Struct(id)),
            ModuleItemId::Enum(id) => Some(GenericTypeId::Enum(id)),
            ModuleItemId::ExternType(id) => Some(GenericTypeId::Extern(id)),
            ModuleItemId::Constant(_)
            | ModuleItemId::Submodule(_)
            | ModuleItemId::TypeAlias(_)
            | ModuleItemId::ImplAlias(_)
            | ModuleItemId::Use(_)
            | ModuleItemId::FreeFunction(_)
            | ModuleItemId::Trait(_)
            | ModuleItemId::Impl(_)
            | ModuleItemId::ExternFunction(_) => None,
        }
    }
}

// Conversion from GenericItemId to LookupItemId.
impl From<GenericItemId> for LookupItemId {
    fn from(item: GenericItemId) -> Self {
        match item {
            GenericItemId::ModuleItem(module_item) => match module_item {
                GenericModuleItemId::FreeFunc(id) => {
                    LookupItemId::ModuleItem(ModuleItemId::FreeFunction(id))
                }
                GenericModuleItemId::ExternFunc(id) => {
                    LookupItemId::ModuleItem(ModuleItemId::ExternFunction(id))
                }
                GenericModuleItemId::TraitFunc(id) => {
                    LookupItemId::TraitItem(TraitItemId::Function(id))
                }
                GenericModuleItemId::ImplFunc(id) => {
                    LookupItemId::ImplItem(ImplItemId::Function(id))
                }
                GenericModuleItemId::Trait(id) => LookupItemId::ModuleItem(ModuleItemId::Trait(id)),
                GenericModuleItemId::Impl(id) => LookupItemId::ModuleItem(ModuleItemId::Impl(id)),
                GenericModuleItemId::Struct(id) => {
                    LookupItemId::ModuleItem(ModuleItemId::Struct(id))
                }
                GenericModuleItemId::Enum(id) => LookupItemId::ModuleItem(ModuleItemId::Enum(id)),
                GenericModuleItemId::ExternType(id) => {
                    LookupItemId::ModuleItem(ModuleItemId::ExternType(id))
                }
                GenericModuleItemId::TypeAlias(id) => {
                    LookupItemId::ModuleItem(ModuleItemId::TypeAlias(id))
                }
                GenericModuleItemId::ImplAlias(id) => {
                    LookupItemId::ModuleItem(ModuleItemId::ImplAlias(id))
                }
            },
            GenericItemId::TraitItem(trait_item) => match trait_item {
                GenericTraitItemId::Type(id) => LookupItemId::TraitItem(TraitItemId::Type(id)),
            },
            GenericItemId::ImplItem(impl_item) => match impl_item {
                GenericImplItemId::Type(id) => LookupItemId::ImplItem(ImplItemId::Type(id)),
            },
        }
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    pub enum StatementItemId {
        Constant(StatementConstId),
        Use(StatementUseId),
    }
}

impl StatementItemId {
    pub fn name(&self, db: &dyn DefsGroup) -> SmolStr {
        match self {
            StatementItemId::Constant(id) => id.name(db),
            StatementItemId::Use(id) => id.name(db),
        }
    }
    pub fn name_stable_ptr(&self, db: &dyn DefsGroup) -> SyntaxStablePtrId {
        match self {
            StatementItemId::Constant(id) => {
                id.lookup_intern(db).1.lookup(db.upcast()).name(db.upcast()).stable_ptr().untyped()
            }
            StatementItemId::Use(id) => {
                id.lookup_intern(db).1.lookup(db.upcast()).name_stable_ptr(db.upcast())
            }
        }
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// Id for direct children of a trait.
    pub enum TraitItemId {
        Function(TraitFunctionId),
        Type(TraitTypeId),
        Constant(TraitConstantId),
        Impl(TraitImplId),
    }
}
impl TraitItemId {
    pub fn name(&self, db: &dyn DefsGroup) -> SmolStr {
        match self {
            TraitItemId::Function(id) => id.name(db),
            TraitItemId::Type(id) => id.name(db),
            TraitItemId::Constant(id) => id.name(db),
            TraitItemId::Impl(id) => id.name(db),
        }
    }
    pub fn trait_id(&self, db: &dyn DefsGroup) -> TraitId {
        match self {
            TraitItemId::Function(id) => id.trait_id(db),
            TraitItemId::Type(id) => id.trait_id(db),
            TraitItemId::Constant(id) => id.trait_id(db),
            TraitItemId::Impl(id) => id.trait_id(db),
        }
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// Id for direct children of an impl.
    pub enum ImplItemId {
        Function(ImplFunctionId),
        Type(ImplTypeDefId),
        Constant(ImplConstantDefId),
        Impl(ImplImplDefId),
    }
}
impl ImplItemId {
    pub fn name(&self, db: &dyn DefsGroup) -> SmolStr {
        match self {
            ImplItemId::Function(id) => id.name(db),
            ImplItemId::Type(id) => id.name(db),
            ImplItemId::Constant(id) => id.name(db),
            ImplItemId::Impl(id) => id.name(db),
        }
    }
    pub fn impl_def_id(&self, db: &dyn DefsGroup) -> ImplDefId {
        match self {
            ImplItemId::Function(id) => id.impl_def_id(db),
            ImplItemId::Type(id) => id.impl_def_id(db),
            ImplItemId::Constant(id) => id.impl_def_id(db),
            ImplItemId::Impl(id) => id.impl_def_id(db),
        }
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// Items for resolver lookups.
    /// These are top items that hold semantic information.
    /// Semantic info lookups should be performed against these items.
    pub enum LookupItemId {
        ModuleItem(ModuleItemId),
        TraitItem(TraitItemId),
        ImplItem(ImplItemId),
    }
}
