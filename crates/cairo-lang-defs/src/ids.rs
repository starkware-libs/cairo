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

use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_syntax::node::ast::TerminalIdentifierGreen;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, NameGreen};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::stable_ptr::SyntaxStablePtr;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::{define_short_id, OptionFrom};
use salsa;
use smol_str::SmolStr;

use crate::db::DefsGroup;

// A trait for an id for a language element.
pub trait LanguageElementId {
    fn module_file_id(&self, db: &dyn DefsGroup) -> ModuleFileId;
    fn untyped_stable_ptr(&self, db: &(dyn DefsGroup + 'static)) -> SyntaxStablePtrId;

    fn parent_module(&self, db: &dyn DefsGroup) -> ModuleId {
        self.module_file_id(db).0
    }
    fn file_index(&self, db: &dyn DefsGroup) -> FileIndex {
        self.module_file_id(db).1
    }
}
pub trait TopLevelLanguageElementId: LanguageElementId {
    fn name(&self, db: &dyn DefsGroup) -> SmolStr;
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.parent_module(db).full_path(db), self.name(db))
    }
}

/// Utility macro for defining an id for a language element.
/// Defines a long id representing some element by a module_id and a stable pointer.
/// Also defines a short id to be used for interning of the long id.
/// Also requires the lookup function name for the lookup fo the long id from the short id,
/// as defined in DefsGroup.
/// Gets an optional parameter `name`. If specified, implements the Named trait using a key_field
/// with this name. See the documentation of 'define_short_id' and `stable_ptr.rs` for more details.
macro_rules! define_language_element_id {
    ($short_id:ident, $long_id:ident, $ast_ty:ty, $lookup:ident $(, $name:ident)?) => {
        define_language_element_id_partial!($short_id, $long_id, $ast_ty, $lookup $(, $name)?);
        impl_top_level_language_element_id!($short_id, $lookup $(, $name)?);
    };
}

/// Utility macro for *partially* defining an id for a language element.
/// This is used by `define_language_element_id` (see its documentation), but doesn't implement
/// TopLevelLanguageElementId for the type.
///
/// Note: prefer to use `define_language_element_id`, unless you need to overwrite the behavior of
/// TopLevelLanguageElementId for the type.
macro_rules! define_language_element_id_partial {
    ($short_id:ident, $long_id:ident, $ast_ty:ty, $lookup:ident $(,$name:ident)?) => {
        #[derive(Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $long_id(pub ModuleFileId, pub <$ast_ty as TypedSyntaxNode>::StablePtr);
        $(
            impl $long_id {
                pub fn $name(&self, db: &dyn DefsGroup) -> SmolStr {
                    let syntax_db = db.upcast();
                    let terminal_green = self.1.name_green(syntax_db);
                    terminal_green.identifier(syntax_db)
                }
            }
            impl<T: ?Sized + cairo_lang_utils::Upcast<dyn DefsGroup + 'static>> cairo_lang_debug::DebugWithDb<T>
                for $long_id
            {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &T) -> std::fmt::Result {
                    let db: &(dyn DefsGroup + 'static) = db.upcast();
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
        )?
        define_short_id!($short_id, $long_id, DefsGroup, $lookup);
        impl $short_id {
            pub fn stable_ptr(self, db: &dyn DefsGroup) -> <$ast_ty as TypedSyntaxNode>::StablePtr {
                db.$lookup(self).1
            }
            $(
                pub fn $name(&self, db: &dyn DefsGroup) -> SmolStr {
                    db.$lookup(*self).name(db)
                }
            )?
        }
        impl LanguageElementId for $short_id {
            fn module_file_id(&self, db: &dyn DefsGroup) -> ModuleFileId {
                db.$lookup(*self).0
            }
            fn untyped_stable_ptr(&self, db: &(dyn DefsGroup + 'static)) -> SyntaxStablePtrId {
                self.stable_ptr(db).untyped()
            }
        }
    };
}

/// A macro to implement TopLevelLanguageElementId for a type. Used by define_language_element_id.
///
/// Note: prefer to use `define_language_element_id`, unless you need to overwrite the behavior of
/// TopLevelLanguageElementId for the type.
macro_rules! impl_top_level_language_element_id {
    ($short_id:ident, $lookup:ident $(,$name:ident)?) => {
        $(
            impl TopLevelLanguageElementId for $short_id {
                fn $name(&self, db: &dyn DefsGroup) -> SmolStr {
                    db.$lookup(*self).name(db)
                }
            }
        )?
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
            fn untyped_stable_ptr(&self, db: &(dyn DefsGroup + 'static)) -> SyntaxStablePtrId {
                match self {
                    $(
                        $enum_name::$variant(id) => id.untyped_stable_ptr(db),
                    )*
                }
            }
        }

        // Conversion from enum to its child.
        $(
            impl OptionFrom<$enum_name> for $variant_ty {
                fn option_from(other: $enum_name) -> Option<Self> {
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
        impl TopLevelLanguageElementId for $enum_name {
            fn name(&self, db: &dyn DefsGroup) -> SmolStr {
                match self {
                    $(
                        $enum_name::$variant(id) => id.name(db),
                    )*
                }
            }
        }

    }
}

/// A trait for getting the internal salsa::InternId of a short id object.
/// This id is unstable across runs and should not be used to anything that is externally visible.
/// This is currently used to pick representative for strongly connected components.
pub trait UnstableSalsaId {
    fn get_internal_id(&self) -> &salsa::InternId;
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
            ModuleId::CrateRoot(id) => db.lookup_intern_crate(*id).0.to_string(),
            ModuleId::Submodule(id) => {
                format!("{}::{}", id.parent_module(db).full_path(db), id.name(db))
            }
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

define_language_element_id_as_enum! {
    /// Id for direct children of a module.
    pub enum ModuleItemId {
        Constant(ConstantId),
        Submodule(SubmoduleId),
        Use(UseId),
        FreeFunction(FreeFunctionId),
        Struct(StructId),
        Enum(EnumId),
        TypeAlias(TypeAliasId),
        Trait(TraitId),
        Impl(ImplId),
        ExternType(ExternTypeId),
        ExternFunction(ExternFunctionId),
    }
}
define_language_element_id!(
    SubmoduleId,
    SubmoduleLongId,
    ast::ItemModule,
    lookup_intern_submodule,
    name
);

define_language_element_id!(
    ConstantId,
    ConstantLongId,
    ast::ItemConstant,
    lookup_intern_constant,
    name
);
define_language_element_id!(UseId, UseLongId, ast::ItemUse, lookup_intern_use, name);
define_language_element_id!(
    FreeFunctionId,
    FreeFunctionLongId,
    ast::FunctionWithBody,
    lookup_intern_free_function,
    name
);

impl UnstableSalsaId for FreeFunctionId {
    fn get_internal_id(&self) -> &salsa::InternId {
        &self.0
    }
}

define_language_element_id!(ImplId, ImplLongId, ast::ItemImpl, lookup_intern_impl, name);
define_language_element_id_partial!(
    ImplFunctionId,
    ImplFunctionLongId,
    ast::FunctionWithBody,
    lookup_intern_impl_function,
    name
);
impl ImplFunctionId {
    pub fn impl_id(&self, db: &dyn DefsGroup) -> ImplId {
        let ImplFunctionLongId(module_file_id, ptr) = db.lookup_intern_impl_function(*self);
        // TODO(spapini): Use a parent function.
        let SyntaxStablePtr::Child{parent, ..} = db.lookup_intern_stable_ptr(ptr.untyped()) else {
            panic!()
        };
        let SyntaxStablePtr::Child{parent, ..} = db.lookup_intern_stable_ptr(parent) else {
            panic!()
        };
        let SyntaxStablePtr::Child{parent, ..} = db.lookup_intern_stable_ptr(parent) else {
            panic!()
        };
        let impl_ptr = ast::ItemImplPtr(parent);
        db.intern_impl(ImplLongId(module_file_id, impl_ptr))
    }
}
impl UnstableSalsaId for ImplFunctionId {
    fn get_internal_id(&self) -> &salsa::InternId {
        &self.0
    }
}
impl TopLevelLanguageElementId for ImplFunctionId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.impl_id(db).name(db), self.name(db))
    }

    fn name(&self, db: &dyn DefsGroup) -> SmolStr {
        db.lookup_intern_impl_function(*self).name(db)
    }
}

define_language_element_id_as_enum! {
    /// Represents a function that has a body.
    pub enum FunctionWithBodyId {
        Free(FreeFunctionId),
        Impl(ImplFunctionId),
    }
}

impl TopLevelLanguageElementId for FunctionWithBodyId {
    fn name(&self, db: &dyn DefsGroup) -> SmolStr {
        match self {
            FunctionWithBodyId::Free(free_function_id) => {
                db.lookup_intern_free_function(*free_function_id).name(db)
            }
            FunctionWithBodyId::Impl(impl_function_id) => {
                db.lookup_intern_impl_function(*impl_function_id).name(db)
            }
        }
    }
}

define_language_element_id!(
    ExternFunctionId,
    ExternFunctionLongId,
    ast::ItemExternFunction,
    lookup_intern_extern_function,
    name
);
define_language_element_id!(StructId, StructLongId, ast::ItemStruct, lookup_intern_struct, name);
define_language_element_id!(EnumId, EnumLongId, ast::ItemEnum, lookup_intern_enum, name);
define_language_element_id!(
    TypeAliasId,
    TypeAliasLongId,
    ast::ItemTypeAlias,
    lookup_intern_type_alias,
    name
);
define_language_element_id!(
    ExternTypeId,
    ExternTypeLongId,
    ast::ItemExternType,
    lookup_intern_extern_type,
    name
);
define_language_element_id!(TraitId, TraitLongId, ast::ItemTrait, lookup_intern_trait, name);
define_language_element_id_partial!(
    TraitFunctionId,
    TraitFunctionLongId,
    ast::TraitItemFunction,
    lookup_intern_trait_function,
    name
);
impl TraitFunctionId {
    pub fn trait_id(&self, db: &dyn DefsGroup) -> TraitId {
        let TraitFunctionLongId(module_file_id, ptr) = db.lookup_intern_trait_function(*self);
        // Trait function ast lies a few levels bellow the trait ast.
        // Fetch the grand grand grand parent.
        // TODO(spapini): Use a parent function.
        let SyntaxStablePtr::Child{parent, ..} = db.lookup_intern_stable_ptr(ptr.untyped()) else {
            panic!()
        };
        let SyntaxStablePtr::Child{parent, ..} = db.lookup_intern_stable_ptr(parent) else {
            panic!()
        };
        let SyntaxStablePtr::Child{parent, ..} = db.lookup_intern_stable_ptr(parent) else {
            panic!()
        };
        let trait_ptr = ast::ItemTraitPtr(parent);
        db.intern_trait(TraitLongId(module_file_id, trait_ptr))
    }
}
impl TopLevelLanguageElementId for TraitFunctionId {
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.trait_id(db).name(db), self.name(db))
    }

    fn name(&self, db: &dyn DefsGroup) -> SmolStr {
        db.lookup_intern_trait_function(*self).name(db)
    }
}

// Struct items.
// TODO(spapini): Override full_path for to include parents, for better debug.
define_language_element_id!(MemberId, MemberLongId, ast::Member, lookup_intern_member, name);
define_language_element_id!(VariantId, VariantLongId, ast::Member, lookup_intern_variant, name);

define_language_element_id_as_enum! {
    /// Id for any variable definition.
    pub enum VarId {
        Param(ParamId),
        Local(LocalVarId),
        // TODO(spapini): Add var from pattern matching.
    }
}

// TODO(spapini): Override full_path for to include parents, for better debug.
define_language_element_id!(ParamId, ParamLongId, ast::Param, lookup_intern_param, name);
define_language_element_id!(
    GenericParamId,
    GenericParamLongId,
    ast::GenericParam,
    lookup_intern_generic_param
);
impl GenericParamLongId {
    pub fn name(&self, db: &dyn SyntaxGroup) -> SmolStr {
        let SyntaxStablePtr::Child {key_fields, .. }=
            db.lookup_intern_stable_ptr(self.1.0) else {
                unreachable!()
            };
        let name_green = TerminalIdentifierGreen(key_fields[0]);
        name_green.identifier(db)
    }
    pub fn kind(&self, db: &dyn SyntaxGroup) -> GenericKind {
        let SyntaxStablePtr::Child { kind, .. } =
            db.lookup_intern_stable_ptr(self.1.0) else {
                unreachable!()
            };
        match kind {
            SyntaxKind::GenericParamType => GenericKind::Type,
            SyntaxKind::GenericParamConst => GenericKind::Const,
            SyntaxKind::GenericParamImpl => GenericKind::Impl,
            _ => unreachable!(),
        }
    }
    /// Retrieves the ID of the generic item holding this generic parameter.
    pub fn generic_item(&self, db: &dyn DefsGroup) -> GenericItemId {
        let SyntaxStablePtr::Child { parent, .. } =
            db.lookup_intern_stable_ptr(self.1.0) else { panic!() };
        let SyntaxStablePtr::Child { parent, .. } =
            db.lookup_intern_stable_ptr(parent) else { panic!() };
        let SyntaxStablePtr::Child { parent, .. } =
            db.lookup_intern_stable_ptr(parent) else { panic!() };
        GenericItemId::from_ptr(db, self.0, parent)
    }
}
impl GenericParamId {
    pub fn name(&self, db: &dyn DefsGroup) -> SmolStr {
        db.lookup_intern_generic_param(*self).name(db.upcast())
    }
    pub fn kind(&self, db: &dyn DefsGroup) -> GenericKind {
        db.lookup_intern_generic_param(*self).kind(db.upcast())
    }
    pub fn generic_item(&self, db: &dyn DefsGroup) -> GenericItemId {
        db.lookup_intern_generic_param(*self).generic_item(db.upcast())
    }
}
impl DebugWithDb<dyn DefsGroup> for GenericParamLongId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn DefsGroup) -> std::fmt::Result {
        write!(f, "GenericParam{}({})", self.kind(db.upcast()), self.name(db.upcast()))
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of an item with generic parameters.
    pub enum GenericItemId {
        FreeFunc(FreeFunctionId),
        ExternFunc(ExternFunctionId),
        TraitFunc(TraitFunctionId),
        ImplFunc(ImplFunctionId),
        Trait(TraitId),
        Impl(ImplId),
        Struct(StructId),
        Enum(EnumId),
        ExternType(ExternTypeId),
        TypeAlias(TypeAliasId),
    }
}
impl GenericItemId {
    pub fn from_ptr(
        db: &dyn DefsGroup,
        module_file: ModuleFileId,
        stable_ptr: SyntaxStablePtrId,
    ) -> Self {
        let SyntaxStablePtr::Child { parent: parent0, kind,.. } =
            db.lookup_intern_stable_ptr(stable_ptr) else { panic!() };
        match kind {
            SyntaxKind::FunctionDeclaration => {
                let SyntaxStablePtr::Child { parent: parent1, kind,.. } =
                    db.lookup_intern_stable_ptr(parent0) else { panic!() };
                match kind {
                    SyntaxKind::FunctionWithBody => {
                        let SyntaxStablePtr::Child { parent: parent2,.. } =
                            db.lookup_intern_stable_ptr(parent1) else { panic!() };

                        match db.lookup_intern_stable_ptr(parent2) {
                            SyntaxStablePtr::Root => GenericItemId::FreeFunc(
                                db.intern_free_function(FreeFunctionLongId(
                                    module_file,
                                    ast::FunctionWithBodyPtr(parent0),
                                )),
                            ),
                            SyntaxStablePtr::Child { kind, .. } => match kind {
                                SyntaxKind::ModuleBody => GenericItemId::FreeFunc(
                                    db.intern_free_function(FreeFunctionLongId(
                                        module_file,
                                        ast::FunctionWithBodyPtr(parent0),
                                    )),
                                ),
                                SyntaxKind::ImplBody => GenericItemId::ImplFunc(
                                    db.intern_impl_function(ImplFunctionLongId(
                                        module_file,
                                        ast::FunctionWithBodyPtr(parent0),
                                    )),
                                ),
                                _ => panic!(),
                            },
                        }
                    }
                    SyntaxKind::ItemExternFunction => {
                        GenericItemId::ExternFunc(db.intern_extern_function(ExternFunctionLongId(
                            module_file,
                            ast::ItemExternFunctionPtr(parent0),
                        )))
                    }
                    SyntaxKind::TraitItemFunction => {
                        GenericItemId::TraitFunc(db.intern_trait_function(TraitFunctionLongId(
                            module_file,
                            ast::TraitItemFunctionPtr(parent0),
                        )))
                    }
                    _ => panic!(),
                }
            }
            SyntaxKind::ItemImpl => GenericItemId::Impl(
                db.intern_impl(ImplLongId(module_file, ast::ItemImplPtr(stable_ptr))),
            ),
            SyntaxKind::ItemTrait => GenericItemId::Trait(
                db.intern_trait(TraitLongId(module_file, ast::ItemTraitPtr(stable_ptr))),
            ),
            SyntaxKind::ItemStruct => GenericItemId::Struct(
                db.intern_struct(StructLongId(module_file, ast::ItemStructPtr(stable_ptr))),
            ),
            SyntaxKind::ItemEnum => GenericItemId::Enum(
                db.intern_enum(EnumLongId(module_file, ast::ItemEnumPtr(stable_ptr))),
            ),
            SyntaxKind::ItemExternType => GenericItemId::ExternType(db.intern_extern_type(
                ExternTypeLongId(module_file, ast::ItemExternTypePtr(stable_ptr)),
            )),
            SyntaxKind::ItemTypeAlias => GenericItemId::TypeAlias(db.intern_type_alias(
                TypeAliasLongId(module_file, ast::ItemTypeAliasPtr(stable_ptr)),
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
}
impl std::fmt::Display for GenericKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericKind::Type => write!(f, "Type"),
            GenericKind::Const => write!(f, "Const"),
            GenericKind::Impl => write!(f, "Impl"),
        }
    }
}

// TODO(spapini): change this to a binding inside a pattern.
// TODO(spapini): Override full_path to include parents, for better debug.
define_language_element_id!(
    LocalVarId,
    LocalVarLongId,
    ast::TerminalIdentifier,
    lookup_intern_local_var
);
impl DebugWithDb<dyn DefsGroup> for LocalVarLongId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn DefsGroup) -> std::fmt::Result {
        let syntax_db = db.upcast();
        let LocalVarLongId(module_file_id, ptr) = self;
        let file_id = db.module_file(*module_file_id).map_err(|_| std::fmt::Error)?;
        let root = db.file_syntax(file_id).map_err(|_| std::fmt::Error)?;
        let text = ast::TerminalIdentifier::from_ptr(syntax_db, &root, *ptr).text(syntax_db);
        write!(f, "LocalVarId({}::{})", module_file_id.0.full_path(db), text)
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// The ID of a function's signature in the code.
    pub enum FunctionSignatureId {
        Free(FreeFunctionId),
        Extern(ExternFunctionId),
        Trait(TraitFunctionId),
        Impl(ImplFunctionId),
    }
}
impl FunctionSignatureId {
    pub fn format(&self, db: &(dyn DefsGroup + 'static)) -> String {
        let function_name = match *self {
            FunctionSignatureId::Free(_) | FunctionSignatureId::Extern(_) => self.name(db).into(),
            FunctionSignatureId::Trait(id) => id.full_path(db),
            FunctionSignatureId::Impl(id) => id.full_path(db),
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
    pub fn format(&self, db: &(dyn DefsGroup + 'static)) -> String {
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
            | ModuleItemId::Use(_)
            | ModuleItemId::FreeFunction(_)
            | ModuleItemId::Trait(_)
            | ModuleItemId::Impl(_)
            | ModuleItemId::ExternFunction(_) => None,
        }
    }
}

define_language_element_id_as_enum! {
    /// Items for resolver lookups.
    /// These are top items that hold semantic information.
    /// Semantic info lookups should be performed against these items.
    pub enum LookupItemId {
        ModuleItem(ModuleItemId),
        // TODO(spapini): Replace with ImplItemId.
        ImplFunction(ImplFunctionId),
    }
}
