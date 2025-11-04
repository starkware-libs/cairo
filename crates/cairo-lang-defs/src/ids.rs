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
use cairo_lang_filesystem::ids::{CrateId, FileId, SmolStrId};
use cairo_lang_syntax::node::ast::TerminalIdentifierGreen;
use cairo_lang_syntax::node::helpers::{GetIdentifier, HasName, NameGreen};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::{Intern, OptionFrom, define_short_id, require};
use salsa::Database;

use crate::db::ModuleData;
use crate::diagnostic_utils::StableLocation;
use crate::plugin::{InlineMacroExprPlugin, MacroPlugin};

// A trait for an id for a language element.
pub trait LanguageElementId<'db> {
    fn module_id(&self, db: &'db dyn Database) -> ModuleId<'db>;
    fn untyped_stable_ptr(&self, db: &'db dyn Database) -> SyntaxStablePtrId<'db>;

    fn parent_module(&self, db: &'db dyn Database) -> ModuleId<'db> {
        self.module_id(db)
    }

    fn stable_location(&self, db: &'db dyn Database) -> StableLocation<'db>;

    fn module_data(&self, db: &'db dyn Database) -> Maybe<ModuleData<'db>> {
        self.parent_module(db).module_data(db)
    }
}

pub trait NamedLanguageElementLongId<'db> {
    fn name(&self, db: &'db dyn Database) -> SmolStrId<'db>;
    fn name_identifier(&'db self, db: &'db dyn Database) -> ast::TerminalIdentifier<'db>;
}
pub trait NamedLanguageElementId<'db>: LanguageElementId<'db> {
    fn name(&self, db: &'db dyn Database) -> SmolStrId<'db>;
    fn name_identifier(&'db self, db: &'db dyn Database) -> ast::TerminalIdentifier<'db>;
}
pub trait TopLevelLanguageElementId<'db>: NamedLanguageElementId<'db> {
    fn full_path(&self, db: &'db dyn Database) -> String {
        format!("{}::{}", self.parent_module(db).full_path(db), self.name(db).long(db))
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
    ($short_id:ident, $long_id:ident, $ast_ty:ty) => {
        define_named_language_element_id!($short_id, $long_id, $ast_ty);
        impl<'db> TopLevelLanguageElementId<'db> for $short_id<'db> {}
    };
}

/// Utility macro for defining an id for a language element, with a name but without full path.
/// This is used by `define_top_level_language_element_id` (see its documentation), but doesn't
/// implement TopLevelLanguageElementId for the type.
///
/// Note: prefer to use `define_top_level_language_element_id`, unless you need to overwrite the
/// behavior of `TopLevelLanguageElementId` for the type.
macro_rules! define_named_language_element_id {
    ($short_id:ident, $long_id:ident, $ast_ty:ty) => {
        define_language_element_id_basic!($short_id, $long_id, $ast_ty);
        impl<'db> cairo_lang_debug::DebugWithDb<'db> for $long_id<'db> {
            type Db = dyn Database;

            fn fmt(
                &self,
                f: &mut std::fmt::Formatter<'_>,
                db: &'db dyn Database,
            ) -> std::fmt::Result {
                let $long_id(module_id, _stable_ptr) = self;
                write!(
                    f,
                    "{}({}::{})",
                    stringify!($short_id),
                    module_id.full_path(db),
                    self.name(db).long(db)
                )
            }
        }
        impl<'db> NamedLanguageElementLongId<'db> for $long_id<'db> {
            fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
                let terminal_green = self.1.name_green(db);
                terminal_green.identifier(db)
            }
            fn name_identifier(&'db self, db: &'db dyn Database) -> ast::TerminalIdentifier<'db> {
                let long = self.1.lookup(db);
                long.name(db)
            }
        }
        impl<'db> NamedLanguageElementId<'db> for $short_id<'db> {
            fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
                self.long(db).name(db)
            }
            fn name_identifier(&'db self, db: &'db dyn Database) -> ast::TerminalIdentifier<'db> {
                let x = self.long(db);
                x.name_identifier(db)
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
    ($short_id:ident, $long_id:ident, $ast_ty:ty) => {
        #[derive(Clone, PartialEq, Eq, Hash, Debug, salsa::Update)]
        pub struct $long_id<'db>(
            pub ModuleId<'db>,
            pub <$ast_ty as TypedSyntaxNode<'db>>::StablePtr,
        );
        define_short_id!($short_id, $long_id<'db>);
        impl<'db> $short_id<'db> {
            pub fn stable_ptr(
                &self,
                db: &'db dyn Database,
            ) -> <$ast_ty as TypedSyntaxNode<'db>>::StablePtr {
                self.long(db).1
            }
        }
        impl<'db> LanguageElementId<'db> for $short_id<'db> {
            fn module_id(&self, db: &'db dyn Database) -> ModuleId<'db> {
                self.long(db).0
            }
            fn untyped_stable_ptr(&self, db: &'db dyn Database) -> SyntaxStablePtrId<'db> {
                let stable_ptr = self.stable_ptr(db);
                stable_ptr.untyped()
            }
            fn stable_location(&self, db: &'db dyn Database) -> StableLocation<'db> {
                let $long_id(_module_id, stable_ptr) = self.long(db);
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
        pub enum $enum_name:ident<$lifetime:lifetime> {
            $($variant:ident ($variant_ty:ty),)*
        }
    ) => {
        toplevel_enum! {
            pub enum $enum_name<$lifetime> {
                $($variant($variant_ty),)*
            }
        }
        define_language_element_id_as_enum! {
            $(#[doc = $doc])*
            pub enum $enum_name<$lifetime> {
                $($variant($variant_ty),)*
            }
        }
    };
    (
        $(#[doc = $doc:expr])*
        pub enum $enum_name:ident<$lifetime:lifetime> {
            $($variant:ident ($variant_ty:ty),)*
        }
    ) => {
        $(#[doc = $doc])*
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
        pub enum $enum_name<$lifetime> {
            $($variant($variant_ty),)*
        }
        impl<'db> cairo_lang_debug::DebugWithDb<'db> for $enum_name<'_> {
            type Db = dyn Database;

            fn fmt(
                &self,
                f: &mut std::fmt::Formatter<'_>,
                db: &'db dyn Database,
            ) -> std::fmt::Result {
                match self {
                    $(
                        $enum_name::$variant(id) => id.fmt(f, db),
                    )*
                }
            }
        }
        impl<'db> LanguageElementId<'db> for $enum_name<'db> {
            fn module_id(&self, db: &'db dyn Database) -> ModuleId<'db> {
                match self {
                    $(
                        $enum_name::$variant(id) => id.module_id(db),
                    )*
                }
            }
            fn untyped_stable_ptr(&self, db: &'db dyn Database) -> SyntaxStablePtrId<'db> {
                match self {
                    $(
                        $enum_name::$variant(id) => id.untyped_stable_ptr(db),
                    )*
                }
            }
            fn stable_location(&self, db: &'db dyn Database) -> StableLocation<'db> {
                 match self {
                    $(
                        $enum_name::$variant(id) => id.stable_location(db),
                    )*
                }
            }

        }

        // Conversion from enum to its child.
        $(
            impl<$lifetime> OptionFrom<$enum_name<$lifetime>> for $variant_ty {
                fn option_from(other: $enum_name<$lifetime>) -> Option<Self> {
                    #[allow(irrefutable_let_patterns)]
                    if let $enum_name::$variant(id) = other {
                        Some(id)
                    } else {
                        None
                    }
                }
            }
        )*
    };
}

macro_rules! toplevel_enum {
    (
        pub enum $enum_name:ident<$lifetime:lifetime> {
            $($variant:ident ($variant_ty:ty),)*
        }
    ) => {
        impl<'db> NamedLanguageElementId<'db> for $enum_name<'db> {
            fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
                match self {
                    $(
                        $enum_name::$variant(id) => id.name(db),
                    )*
                }
            }
            fn name_identifier(&'db self, db: &'db dyn Database) -> ast::TerminalIdentifier<'db> {
                match self {
                    $(
                        $enum_name::$variant(id) => id.name_identifier(db),
                    )*
                }
            }
        }
        impl<'db> TopLevelLanguageElementId<'db> for $enum_name<'db> {}
    }
}

/// Id for a module. Either the root module of a crate, or a submodule.
// TODO(eytan-starkware): Track this type to improve performance.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub enum ModuleId<'db> {
    CrateRoot(CrateId<'db>),
    Submodule(SubmoduleId<'db>),
    // Macros at the item level are expanded into full files that can be considered an anonymous
    // module.
    MacroCall {
        /// The id of the macro call.
        id: MacroCallId<'db>,
        /// The file id of the text generated by the macro call.
        generated_file_id: FileId<'db>,
        /// The call was to the `expose!` macro.
        is_expose: bool,
    },
}
impl<'db> ModuleId<'db> {
    pub fn full_path(&self, db: &dyn Database) -> String {
        match self {
            ModuleId::CrateRoot(id) => id.long(db).name().to_string(db),
            ModuleId::Submodule(id) => {
                format!("{}::{}", id.parent_module(db).full_path(db), id.name(db).long(db))
            }
            ModuleId::MacroCall { id, .. } => {
                format!("{}::{}", id.parent_module(db).full_path(db), self.name(db).long(db))
            }
        }
    }
    pub fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        match self {
            ModuleId::CrateRoot(id) => id.long(db).name(),
            ModuleId::Submodule(id) => id.name(db),
            ModuleId::MacroCall { id, .. } => {
                id.stable_ptr(db).lookup(db).as_syntax_node().get_text_without_trivia(db)
            }
        }
    }
    pub fn owning_crate(&self, db: &'db dyn Database) -> CrateId<'db> {
        match self {
            ModuleId::CrateRoot(crate_id) => *crate_id,
            ModuleId::Submodule(submodule) => {
                let parent: ModuleId<'db> = submodule.parent_module(db);
                parent.owning_crate(db)
            }
            ModuleId::MacroCall { id, .. } => id.parent_module(db).owning_crate(db),
        }
    }
    pub fn module_data(&self, db: &'db dyn Database) -> Maybe<ModuleData<'db>> {
        crate::db::module_data(db, *self)
    }
}
impl<'db> DebugWithDb<'db> for ModuleId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        write!(f, "ModuleId({})", self.full_path(db))
    }
}

/// An id for a file defined out of the filesystem crate, for files generated by plugins.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PluginGeneratedFileLongId<'db> {
    /// The module that the file was generated from.
    pub module_id: ModuleId<'db>,
    /// The stable pointer the file was generated from being ran on.
    pub stable_ptr: SyntaxStablePtrId<'db>,
    /// The name of the generated file to differentiate between different generated files.
    pub name: String,
}
define_short_id!(PluginGeneratedFileId, PluginGeneratedFileLongId<'db>);

/// An ID allowing for interning the [`MacroPlugin`] into Salsa database.
#[derive(Clone, Debug)]
pub struct MacroPluginLongId(pub Arc<dyn MacroPlugin>);

impl MacroPlugin for MacroPluginLongId {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        item_ast: ast::ModuleItem<'db>,
        metadata: &crate::plugin::MacroPluginMetadata<'_>,
    ) -> crate::plugin::PluginResult<'db> {
        self.0.generate_code(db, item_ast, metadata)
    }

    fn declared_attributes<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        self.0.declared_attributes(db)
    }

    fn declared_derives<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        self.0.declared_derives(db)
    }

    fn executable_attributes<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        self.0.executable_attributes(db)
    }

    fn phantom_type_attributes<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        self.0.phantom_type_attributes(db)
    }

    fn plugin_type_id(&self) -> std::any::TypeId {
        // Ensure the implementation for `MacroPluginLongId` returns the same value
        // as the underlying plugin object.
        self.0.plugin_type_id()
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

define_short_id!(MacroPluginId, MacroPluginLongId);

/// An ID allowing for interning the [`InlineMacroExprPlugin`] into Salsa database.
#[derive(Clone, Debug)]
pub struct InlineMacroExprPluginLongId(pub Arc<dyn InlineMacroExprPlugin>);

impl InlineMacroExprPlugin for InlineMacroExprPluginLongId {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        item_ast: &ast::ExprInlineMacro<'db>,
        metadata: &crate::plugin::MacroPluginMetadata<'_>,
    ) -> crate::plugin::InlinePluginResult<'db> {
        self.0.generate_code(db, item_ast, metadata)
    }

    fn documentation(&self) -> Option<String> {
        self.0.documentation()
    }

    fn plugin_type_id(&self) -> std::any::TypeId {
        // Ensure the implementation for `InlineMacroExprPluginLongId` returns the same value
        // as the underlying plugin object.
        self.0.plugin_type_id()
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

define_short_id!(InlineMacroExprPluginId, InlineMacroExprPluginLongId);

define_language_element_id_as_enum! {
    #[toplevel]
    /// Id for direct children of a module.
    pub enum ModuleItemId<'db> {
        Constant(ConstantId<'db>),
        Submodule(SubmoduleId<'db>),
        Use(UseId<'db>),
        FreeFunction(FreeFunctionId<'db>),
        Struct(StructId<'db>),
        Enum(EnumId<'db>),
        TypeAlias(ModuleTypeAliasId<'db>),
        ImplAlias(ImplAliasId<'db>),
        Trait(TraitId<'db>),
        Impl(ImplDefId<'db>),
        ExternType(ExternTypeId<'db>),
        ExternFunction(ExternFunctionId<'db>),
        MacroDeclaration(MacroDeclarationId<'db>),
    }
}

/// Id for an item that can be brought into scope with a `use` statement.
/// Basically [`ModuleItemId`] without [`UseId`] and with [`VariantId`] and [`CrateId`].
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub enum ImportableId<'db> {
    Constant(ConstantId<'db>),
    Submodule(SubmoduleId<'db>),
    Crate(CrateId<'db>),
    FreeFunction(FreeFunctionId<'db>),
    Struct(StructId<'db>),
    Enum(EnumId<'db>),
    Variant(VariantId<'db>),
    TypeAlias(ModuleTypeAliasId<'db>),
    ImplAlias(ImplAliasId<'db>),
    Trait(TraitId<'db>),
    Impl(ImplDefId<'db>),
    ExternType(ExternTypeId<'db>),
    ExternFunction(ExternFunctionId<'db>),
    MacroDeclaration(MacroDeclarationId<'db>),
}

define_top_level_language_element_id!(SubmoduleId, SubmoduleLongId, ast::ItemModule<'db>);
impl<'db> UnstableSalsaId for SubmoduleId<'db> {
    fn get_internal_id(&self) -> salsa::Id {
        self.0
    }
}

define_top_level_language_element_id!(ConstantId, ConstantLongId, ast::ItemConstant<'db>);
define_language_element_id_basic!(GlobalUseId, GlobalUseLongId, ast::UsePathStar<'db>);
define_top_level_language_element_id!(UseId, UseLongId, ast::UsePathLeaf<'db>);
define_top_level_language_element_id!(
    FreeFunctionId,
    FreeFunctionLongId,
    ast::FunctionWithBody<'db>
);

define_top_level_language_element_id!(
    MacroDeclarationId,
    MacroDeclarationLongId,
    ast::ItemMacroDeclaration<'db>
);

define_language_element_id_basic!(MacroCallId, MacroCallLongId, ast::ItemInlineMacro<'db>);

impl<'db> UnstableSalsaId for MacroCallId<'db> {
    fn get_internal_id(&self) -> salsa::Id {
        self.0
    }
}

impl<'db> UnstableSalsaId for FreeFunctionId<'db> {
    fn get_internal_id(&self) -> salsa::Id {
        self.0
    }
}

// --- Impls ---
define_top_level_language_element_id!(ImplDefId, ImplDefLongId, ast::ItemImpl<'db>);
impl<'db> UnstableSalsaId for ImplDefId<'db> {
    fn get_internal_id(&self) -> salsa::Id {
        self.0
    }
}

// --- Impl type items ---
define_named_language_element_id!(ImplTypeDefId, ImplTypeDefLongId, ast::ItemTypeAlias<'db>);
impl<'db> ImplTypeDefId<'db> {
    pub fn impl_def_id(&self, db: &'db dyn Database) -> ImplDefId<'db> {
        let ImplTypeDefLongId(module_id, ptr) = self.long(db).clone();

        // Impl type ast lies 3 levels below the impl ast.
        let impl_ptr = ast::ItemImplPtr(ptr.untyped().nth_parent(db, 3));
        ImplDefLongId(module_id, impl_ptr).intern(db)
    }
}
impl<'db> TopLevelLanguageElementId<'db> for ImplTypeDefId<'db> {
    fn full_path(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.impl_def_id(db).full_path(db), self.name(db).long(db))
    }
}

// --- Impl constant items ---
define_named_language_element_id!(ImplConstantDefId, ImplConstantDefLongId, ast::ItemConstant<'db>);
impl<'db> ImplConstantDefId<'db> {
    pub fn impl_def_id(&self, db: &'db dyn Database) -> ImplDefId<'db> {
        let ImplConstantDefLongId(module_id, ptr) = self.long(db).clone();

        // Impl constant ast lies 3 levels below the impl ast.
        let impl_ptr = ast::ItemImplPtr(ptr.untyped().nth_parent(db, 3));
        ImplDefLongId(module_id, impl_ptr).intern(db)
    }
}
impl<'db> TopLevelLanguageElementId<'db> for ImplConstantDefId<'db> {
    fn full_path(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.impl_def_id(db).full_path(db), self.name(db).long(db))
    }
}

// --- Impl Impl items ---
define_named_language_element_id!(ImplImplDefId, ImplImplDefLongId, ast::ItemImplAlias<'db>);
impl<'db> ImplImplDefId<'db> {
    pub fn impl_def_id(&self, db: &'db dyn Database) -> ImplDefId<'db> {
        let ImplImplDefLongId(module_id, ptr) = self.long(db).clone();

        // Impl constant ast lies 3 levels below the impl ast.
        let impl_ptr = ast::ItemImplPtr(ptr.untyped().nth_parent(db, 3));
        ImplDefLongId(module_id, impl_ptr).intern(db)
    }
}
impl<'db> TopLevelLanguageElementId<'db> for ImplImplDefId<'db> {
    fn full_path(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.impl_def_id(db).full_path(db), self.name(db).long(db))
    }
}

// --- Impl functions ---
define_named_language_element_id!(ImplFunctionId, ImplFunctionLongId, ast::FunctionWithBody<'db>);
impl<'db> ImplFunctionId<'db> {
    pub fn impl_def_id(&self, db: &'db dyn Database) -> ImplDefId<'db> {
        let ImplFunctionLongId(module_id, ptr) = self.long(db).clone();

        // Impl function ast lies 3 levels below the impl ast.
        let impl_ptr = ast::ItemImplPtr(ptr.untyped().nth_parent(db, 3));
        ImplDefLongId(module_id, impl_ptr).intern(db)
    }
}
impl<'db> UnstableSalsaId for ImplFunctionId<'db> {
    fn get_internal_id(&self) -> salsa::Id {
        self.0
    }
}
impl<'db> TopLevelLanguageElementId<'db> for ImplFunctionId<'db> {
    fn full_path(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.impl_def_id(db).full_path(db), self.name(db).long(db))
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// Represents a function that has a body.
    pub enum FunctionWithBodyId<'db> {
        Free(FreeFunctionId<'db>),
        Impl(ImplFunctionId<'db>),
        Trait(TraitFunctionId<'db>),
    }
}

define_top_level_language_element_id!(
    ExternFunctionId,
    ExternFunctionLongId,
    ast::ItemExternFunction<'db>
);
define_top_level_language_element_id!(StructId, StructLongId, ast::ItemStruct<'db>);
define_top_level_language_element_id!(EnumId, EnumLongId, ast::ItemEnum<'db>);
define_top_level_language_element_id!(
    ModuleTypeAliasId,
    ModuleTypeAliasLongId,
    ast::ItemTypeAlias<'db>
);
define_top_level_language_element_id!(ImplAliasId, ImplAliasLongId, ast::ItemImplAlias<'db>);
impl<'db> UnstableSalsaId for ImplAliasId<'db> {
    fn get_internal_id(&self) -> salsa::Id {
        self.0
    }
}
define_top_level_language_element_id!(ExternTypeId, ExternTypeLongId, ast::ItemExternType<'db>);

// --- Trait ---
define_top_level_language_element_id!(TraitId, TraitLongId, ast::ItemTrait<'db>);

// --- Trait type items ---
define_named_language_element_id!(TraitTypeId, TraitTypeLongId, ast::TraitItemType<'db>);
impl<'db> TraitTypeId<'db> {
    pub fn trait_id(&self, db: &'db dyn Database) -> TraitId<'db> {
        let TraitTypeLongId(module_id, ptr) = self.long(db).clone();
        // Trait type ast lies 3 levels below the trait ast.
        let trait_ptr = ast::ItemTraitPtr(ptr.untyped().nth_parent(db, 3));
        TraitLongId(module_id, trait_ptr).intern(db)
    }
}
impl<'db> TopLevelLanguageElementId<'db> for TraitTypeId<'db> {
    fn full_path(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.trait_id(db).full_path(db), self.name(db).long(db))
    }
}
impl<'db> UnstableSalsaId for TraitTypeId<'db> {
    fn get_internal_id(&self) -> salsa::Id {
        self.0
    }
}

// --- Trait constant items ---
define_named_language_element_id!(
    TraitConstantId,
    TraitConstantLongId,
    ast::TraitItemConstant<'db>
);
impl<'db> TraitConstantId<'db> {
    pub fn trait_id(&self, db: &'db dyn Database) -> TraitId<'db> {
        let TraitConstantLongId(module_id, ptr) = self.long(db).clone();
        // Trait constant ast lies 3 levels below the trait ast.
        let trait_ptr = ast::ItemTraitPtr(ptr.untyped().nth_parent(db, 3));
        TraitLongId(module_id, trait_ptr).intern(db)
    }
}
impl<'db> TopLevelLanguageElementId<'db> for TraitConstantId<'db> {
    fn full_path(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.trait_id(db).full_path(db), self.name(db).long(db))
    }
}

// --- Trait impl items ---
define_named_language_element_id!(TraitImplId, TraitImplLongId, ast::TraitItemImpl<'db>);
impl<'db> TraitImplId<'db> {
    pub fn trait_id(&self, db: &'db dyn Database) -> TraitId<'db> {
        let TraitImplLongId(module_id, ptr) = self.long(db).clone();
        // Trait impl ast lies 3 levels below the trait ast.
        let trait_ptr = ast::ItemTraitPtr(ptr.untyped().nth_parent(db, 3));
        TraitLongId(module_id, trait_ptr).intern(db)
    }
}
impl<'db> TopLevelLanguageElementId<'db> for TraitImplId<'db> {
    fn full_path(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.trait_id(db).full_path(db), self.name(db).long(db))
    }
}
impl<'db> UnstableSalsaId for TraitImplId<'db> {
    fn get_internal_id(&self) -> salsa::Id {
        self.0
    }
}

// --- Trait functions ---
define_named_language_element_id!(
    TraitFunctionId,
    TraitFunctionLongId,
    ast::TraitItemFunction<'db>
);
impl<'db> TraitFunctionId<'db> {
    pub fn trait_id(&self, db: &'db dyn Database) -> TraitId<'db> {
        let TraitFunctionLongId(module_id, ptr) = self.long(db).clone();
        // Trait function ast lies 3 levels below the trait ast.
        let trait_ptr = ast::ItemTraitPtr(ptr.untyped().nth_parent(db, 3));
        TraitLongId(module_id, trait_ptr).intern(db)
    }
}
impl<'db> TopLevelLanguageElementId<'db> for TraitFunctionId<'db> {
    fn full_path(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.trait_id(db).full_path(db), self.name(db).long(db))
    }
}

// --- Struct items ---
define_named_language_element_id!(MemberId, MemberLongId, ast::Member<'db>);
impl<'db> MemberId<'db> {
    pub fn struct_id(&self, db: &'db dyn Database) -> StructId<'db> {
        let MemberLongId(module_id, ptr) = self.long(db).clone();
        let struct_ptr = ast::ItemStructPtr(ptr.untyped().nth_parent(db, 2));
        StructLongId(module_id, struct_ptr).intern(db)
    }
}

impl<'db> TopLevelLanguageElementId<'db> for MemberId<'db> {
    fn full_path(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.struct_id(db).full_path(db), self.name(db).long(db))
    }
}

// --- Enum variants ---
define_named_language_element_id!(VariantId, VariantLongId, ast::Variant<'db>);
impl<'db> VariantId<'db> {
    pub fn enum_id(&self, db: &'db dyn Database) -> EnumId<'db> {
        let VariantLongId(module_id, ptr) = self.long(db).clone();
        let struct_ptr = ast::ItemEnumPtr(ptr.untyped().nth_parent(db, 2));
        EnumLongId(module_id, struct_ptr).intern(db)
    }
}

impl<'db> TopLevelLanguageElementId<'db> for VariantId<'db> {
    fn full_path(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.enum_id(db).full_path(db), self.name(db).long(db))
    }
}

define_language_element_id_as_enum! {
    /// Id for any variable definition.
    pub enum VarId<'db> {
        Param(ParamId<'db>),
        Local(LocalVarId<'db>),
        Item(StatementItemId<'db>),
        // TODO(spapini): Add var from pattern matching.
    }
}

// TODO(spapini): Override full_path to include parents, for better debug.
define_top_level_language_element_id!(ParamId, ParamLongId, ast::Param<'db>);
define_language_element_id_basic!(GenericParamId, GenericParamLongId, ast::GenericParam<'db>);
impl<'db> GenericParamLongId<'db> {
    pub fn name(&self, db: &'db dyn Database) -> Option<SmolStrId<'db>> {
        let node = self.1.0.0;
        assert!(!node.is_root());
        let key_fields = node.key_fields(db);
        let kind = node.kind(db);
        require(!matches!(
            kind,
            SyntaxKind::GenericParamImplAnonymous | SyntaxKind::GenericParamNegativeImpl
        ))?;

        let name_green = TerminalIdentifierGreen(key_fields[0]);
        Some(name_green.identifier(db))
    }

    pub fn debug_name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        self.name(db).unwrap_or(SmolStrId::from(db, "_"))
    }
    pub fn kind(&self, db: &dyn Database) -> GenericKind {
        let node = self.1.0.0;
        assert!(!node.is_root());
        let kind = node.kind(db);
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
    pub fn generic_item<'s, 'd: 's>(&'s self, db: &'d dyn Database) -> GenericItemId<'s> {
        let item_ptr = self.1.0.nth_parent(db, 3);
        GenericItemId::from_ptr(db, self.0, item_ptr)
    }

    /// Returns `true` if the generic parameter has type constraints syntax.
    pub fn has_type_constraints_syntax(&self, db: &dyn Database) -> bool {
        let param = ast::GenericParamPtr(self.1.0).lookup(db);
        match param {
            ast::GenericParam::Type(_) => false,
            ast::GenericParam::Const(_) => false,
            ast::GenericParam::ImplNamed(imp) => {
                matches!(
                    imp.type_constrains(db),
                    ast::OptionAssociatedItemConstraints::AssociatedItemConstraints(_)
                )
            }
            ast::GenericParam::ImplAnonymous(imp) => {
                matches!(
                    imp.type_constrains(db),
                    ast::OptionAssociatedItemConstraints::AssociatedItemConstraints(_)
                )
            }
            ast::GenericParam::NegativeImpl(_) => false,
        }
    }
}
impl<'db> GenericParamId<'db> {
    pub fn name(&self, db: &'db dyn Database) -> Option<SmolStrId<'db>> {
        self.long(db).name(db)
    }
    pub fn debug_name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        self.long(db).debug_name(db)
    }
    pub fn format(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        let long_ids = self.long(db);
        let node = long_ids.1.0.0;
        assert!(!node.is_root());
        let key_fields = node.key_fields(db);
        let kind = node.kind(db);

        if matches!(
            kind,
            SyntaxKind::GenericParamImplAnonymous | SyntaxKind::GenericParamNegativeImpl
        ) {
            // For anonymous impls print the declaration.
            return self.stable_location(db).syntax_node(db).get_text_without_trivia(db);
        }

        let name_green = TerminalIdentifierGreen(key_fields[0]);
        name_green.identifier(db)
    }

    pub fn kind(&self, db: &dyn Database) -> GenericKind {
        self.long(db).kind(db)
    }
    pub fn generic_item(&self, db: &'db dyn Database) -> GenericItemId<'db> {
        self.long(db).generic_item(db)
    }
}

impl<'db> UnstableSalsaId for GenericParamId<'db> {
    fn get_internal_id(&self) -> salsa::Id {
        self.0
    }
}

impl<'db> DebugWithDb<'db> for GenericParamLongId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        write!(
            f,
            "GenericParam{}({}::{})",
            self.kind(db),
            self.generic_item(db).full_path(db),
            self.debug_name(db).long(db)
        )
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of a module item with generic parameters.
    pub enum GenericModuleItemId<'db> {
        FreeFunc(FreeFunctionId<'db>),
        ExternFunc(ExternFunctionId<'db>),
        TraitFunc(TraitFunctionId<'db>),
        ImplFunc(ImplFunctionId<'db>),
        Trait(TraitId<'db>),
        Impl(ImplDefId<'db>),
        Struct(StructId<'db>),
        Enum(EnumId<'db>),
        ExternType(ExternTypeId<'db>),
        TypeAlias(ModuleTypeAliasId<'db>),
        ImplAlias(ImplAliasId<'db>),
    }
}
define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of a trait item with generic parameters.
    pub enum GenericTraitItemId<'db> {
        Type(TraitTypeId<'db>),
    }
}
define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of a impl item with generic parameters.
    pub enum GenericImplItemId<'db> {
        Type(ImplTypeDefId<'db>),
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of an item with generic parameters.
    pub enum GenericItemId<'db> {
        ModuleItem(GenericModuleItemId<'db>),
        TraitItem(GenericTraitItemId<'db>),
        ImplItem(GenericImplItemId<'db>),
    }
}
impl<'db> GenericItemId<'db> {
    pub fn from_ptr(
        db: &'db dyn Database,
        module_file: ModuleId<'db>,
        stable_ptr: SyntaxStablePtrId<'db>,
    ) -> Self {
        let node = stable_ptr.0;
        let parent0 = node.parent(db).expect("GenericItem should have a parent");
        let kind = node.kind(db);
        match kind {
            SyntaxKind::FunctionDeclaration => {
                let parent1 = parent0.parent(db).expect("FunctionDeclaration parent should exist");
                let kind = parent0.kind(db);
                match kind {
                    SyntaxKind::FunctionWithBody => {
                        // `FunctionWithBody` must be at least 2 levels below the root, and thus
                        // `parent1.parent()` is safe.
                        let parent0_ptr = SyntaxStablePtrId(parent0);
                        match parent1.parent(db).map(|p| p.kind(db)) {
                            // SyntaxFile is root level (file level)
                            Some(SyntaxKind::SyntaxFile) | Some(SyntaxKind::ModuleBody) => {
                                GenericItemId::ModuleItem(GenericModuleItemId::FreeFunc(
                                    FreeFunctionLongId(
                                        module_file,
                                        ast::FunctionWithBodyPtr(parent0_ptr),
                                    )
                                    .intern(db),
                                ))
                            }
                            Some(SyntaxKind::ImplBody) => {
                                GenericItemId::ModuleItem(GenericModuleItemId::ImplFunc(
                                    ImplFunctionLongId(
                                        module_file,
                                        ast::FunctionWithBodyPtr(parent0_ptr),
                                    )
                                    .intern(db),
                                ))
                            }
                            _ => panic!(
                                "Got bad syntax kind @ parent of {}. {:?}",
                                parent1.kind(db),
                                kind
                            ),
                        }
                    }
                    SyntaxKind::ItemExternFunction => {
                        GenericItemId::ModuleItem(GenericModuleItemId::ExternFunc(
                            ExternFunctionLongId(
                                module_file,
                                ast::ItemExternFunctionPtr(SyntaxStablePtrId(parent0)),
                            )
                            .intern(db),
                        ))
                    }
                    SyntaxKind::TraitItemFunction => {
                        GenericItemId::ModuleItem(GenericModuleItemId::TraitFunc(
                            TraitFunctionLongId(
                                module_file,
                                ast::TraitItemFunctionPtr(SyntaxStablePtrId(parent0)),
                            )
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
                match parent0.kind(db) {
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
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
define_language_element_id_basic!(LocalVarId, LocalVarLongId, ast::TerminalIdentifier<'db>);
impl<'db> DebugWithDb<'db> for LocalVarLongId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        let LocalVarLongId(module_id, ptr) = self;
        let text = ptr.lookup(db).text(db).long(db);
        write!(f, "LocalVarId({}::{})", module_id.full_path(db), text)
    }
}

define_top_level_language_element_id!(
    StatementConstId,
    StatementConstLongId,
    ast::ItemConstant<'db>
);

define_top_level_language_element_id!(StatementUseId, StatementUseLongId, ast::UsePathLeaf<'db>);

define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of a function's signature in the code.
    pub enum FunctionTitleId<'db> {
        Free(FreeFunctionId<'db>),
        Extern(ExternFunctionId<'db>),
        Trait(TraitFunctionId<'db>),
        Impl(ImplFunctionId<'db>),
    }
}
impl<'db> FunctionTitleId<'db> {
    pub fn format(&self, db: &dyn Database) -> String {
        let function_name = match *self {
            FunctionTitleId::Free(_) | FunctionTitleId::Extern(_) => self.name(db).to_string(db),
            FunctionTitleId::Trait(id) => id.full_path(db),
            FunctionTitleId::Impl(id) => id.full_path(db),
        };
        format!("{}::{}", self.parent_module(db).full_path(db), function_name)
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// Generic type ids enum.
    pub enum GenericTypeId<'db> {
        Struct(StructId<'db>),
        Enum(EnumId<'db>),
        Extern(ExternTypeId<'db>),
        // TODO(spapini): associated types in impls.
    }
}
impl<'db> GenericTypeId<'db> {
    pub fn format(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.parent_module(db).full_path(db), self.name(db).long(db))
    }
}

/// Conversion from ModuleItemId to GenericTypeId.
impl<'db> OptionFrom<ModuleItemId<'db>> for GenericTypeId<'db> {
    fn option_from(item: ModuleItemId<'db>) -> Option<Self> {
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
            | ModuleItemId::ExternFunction(_)
            | ModuleItemId::MacroDeclaration(_) => None,
        }
    }
}

// Conversion from GenericItemId to LookupItemId.
impl<'db> From<GenericItemId<'db>> for LookupItemId<'db> {
    fn from(item: GenericItemId<'db>) -> Self {
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
    pub enum StatementItemId<'db> {
        Constant(StatementConstId<'db>),
        Use(StatementUseId<'db>),
    }
}

impl<'db> StatementItemId<'db> {
    pub fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        match self {
            StatementItemId::Constant(id) => id.name(db),
            StatementItemId::Use(id) => id.name(db),
        }
    }
    pub fn name_stable_ptr(&self, db: &'db dyn Database) -> SyntaxStablePtrId<'db> {
        match self {
            StatementItemId::Constant(id) => {
                let id: &StatementConstId<'db> = id;
                let item_id = id.long(db).1.lookup(db);
                item_id.name(db).stable_ptr(db).untyped()
            }
            StatementItemId::Use(id) => {
                let item_id = id.long(db).1.lookup(db);
                item_id.name_stable_ptr(db)
            }
        }
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// Id for direct children of a trait.
    pub enum TraitItemId<'db> {
        Function(TraitFunctionId<'db>),
        Type(TraitTypeId<'db>),
        Constant(TraitConstantId<'db>),
        Impl(TraitImplId<'db>),
    }
}
impl<'db> TraitItemId<'db> {
    pub fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        match self {
            TraitItemId::Function(id) => id.name(db),
            TraitItemId::Type(id) => id.name(db),
            TraitItemId::Constant(id) => id.name(db),
            TraitItemId::Impl(id) => id.name(db),
        }
    }
    pub fn trait_id(&self, db: &'db dyn Database) -> TraitId<'db> {
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
    pub enum ImplItemId<'db> {
        Function(ImplFunctionId<'db>),
        Type(ImplTypeDefId<'db>),
        Constant(ImplConstantDefId<'db>),
        Impl(ImplImplDefId<'db>),
    }
}
impl<'db> ImplItemId<'db> {
    pub fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        match self {
            ImplItemId::Function(id) => id.name(db),
            ImplItemId::Type(id) => id.name(db),
            ImplItemId::Constant(id) => id.name(db),
            ImplItemId::Impl(id) => id.name(db),
        }
    }
    pub fn impl_def_id(&self, db: &'db dyn Database) -> ImplDefId<'db> {
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
    pub enum LookupItemId<'db> {
        ModuleItem(ModuleItemId<'db>),
        TraitItem(TraitItemId<'db>),
        ImplItem(ImplItemId<'db>),
    }
}
