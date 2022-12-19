// The following ids represent all the definitions in the code.
// Roughly, this refers to the first appearance of each identifier.
// Everything that can be returned by "Go to definition" is a definition.
//
// Examples:
// * let x = 5.
// Has a definition for the variable "x".
// * func foo<T>(a: T){ return (); }.
// Has 3 definitions:
//   * Function "foo".
//   * Generic parameter "T" (only the first occurrence of "T").
//   * Function parameter "a".
// * trait MyTrait{ func foo() -> (); }
// Has 2 definitions:
//   * Trait "MyTrait"
//   * TraitFunction "foo".
// * impl A for MyTrait{ func foo() -> (){...} }
// Has 2 definitions:
//   * Impl "A"
//   * ImplFunction "foo".
//
// Call sites, variable usages, assignments, etc. are NOT definitions.

use db_utils::define_short_id;
use debug::debug::DebugWithDb;
use filesystem::ids::{CrateId, FileId};
use smol_str::SmolStr;
use syntax::node::helpers::GetIdentifier;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::stable_ptr::SyntaxStablePtr;
use syntax::node::{ast, Terminal, TypedSyntaxNode};
use utils::OptionFrom;

use crate::db::DefsGroup;

// A trait for an id for a language element.
pub trait LanguageElementId {
    fn module(&self, db: &dyn DefsGroup) -> ModuleId;
    fn file_index(&self, db: &dyn DefsGroup) -> FileIndex;
    fn module_file(&self, db: &dyn DefsGroup) -> ModuleFileId {
        ModuleFileId(self.module(db), self.file_index(db))
    }
    fn untyped_stable_ptr(&self, db: &(dyn DefsGroup + 'static)) -> SyntaxStablePtrId;
}
pub trait TopLevelLanguageElementId: LanguageElementId {
    fn name(&self, db: &dyn DefsGroup) -> SmolStr;
    fn full_path(&self, db: &dyn DefsGroup) -> String {
        format!("{}::{}", self.module(db).full_path(db), self.name(db))
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
            impl<T: ?Sized + db_utils::Upcast<dyn DefsGroup + 'static>> debug::DebugWithDb<T>
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
            fn module(&self, db: &dyn DefsGroup) -> ModuleId {
                db.$lookup(*self).0.0
            }
            fn file_index(&self, db: &dyn DefsGroup) -> FileIndex {
                db.$lookup(*self).0.1
            }
            fn untyped_stable_ptr(&self, db: &(dyn DefsGroup + 'static)) -> SyntaxStablePtrId {
                self.stable_ptr(db).untyped()
            }
        }
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
        impl<T: ?Sized + db_utils::Upcast<dyn DefsGroup + 'static>> debug::DebugWithDb<T>
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
            fn module(&self, db: &dyn DefsGroup) -> ModuleId {
                match self {
                    $(
                        $enum_name::$variant(id) => id.module(db),
                    )*
                }
            }
            fn file_index(&self, db: &dyn DefsGroup) -> FileIndex {
                match self {
                    $(
                        $enum_name::$variant(id) => id.file_index(db),
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

/// Id for a module. Either the root module of a crate, or a submodule.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModuleId {
    CrateRoot(CrateId),
    Submodule(SubmoduleId),
    VirtualSubmodule(VirtualSubmoduleId),
}
impl ModuleId {
    pub fn full_path(&self, db: &dyn DefsGroup) -> String {
        match self {
            ModuleId::CrateRoot(id) => db.lookup_intern_crate(*id).0.to_string(),
            ModuleId::Submodule(id) => {
                format!("{}::{}", id.module(db).full_path(db), id.name(db))
            }
            ModuleId::VirtualSubmodule(virtual_submodule_id) => {
                let virtual_submodule = db.lookup_intern_virtual_submodule(*virtual_submodule_id);
                format!("{}::{}", virtual_submodule.parent.full_path(db), virtual_submodule.name)
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

/// A virtual sub module is a module create by a macro plugin. All plugin generated code is placed
/// in such submodules to avoid namespace pollution.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VirtualSubmodule {
    pub name: SmolStr,
    pub parent: ModuleId,
    pub file: FileId,
}
define_short_id!(VirtualSubmoduleId, VirtualSubmodule, DefsGroup, lookup_intern_virtual_submodule);

define_language_element_id_as_enum! {
    /// Id for direct children of a module.
    pub enum ModuleItemId {
        Submodule(SubmoduleId),
        Use(UseId),
        FreeFunction(FreeFunctionId),
        Struct(StructId),
        Enum(EnumId),
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

impl SubmoduleId {
    pub fn module_file_id(&self, db: &dyn DefsGroup) -> ModuleFileId {
        db.lookup_intern_submodule(*self).0
    }
}

define_language_element_id!(UseId, UseLongId, ast::ItemUse, lookup_intern_use, name);
define_language_element_id!(
    FreeFunctionId,
    FreeFunctionLongId,
    ast::ItemFreeFunction,
    lookup_intern_free_function,
    name
);
impl PartialOrd for FreeFunctionId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl Ord for FreeFunctionId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

define_language_element_id!(
    ImplFunctionId,
    ImplFunctionLongId,
    ast::ItemFreeFunction,
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
    ExternTypeId,
    ExternTypeLongId,
    ast::ItemExternType,
    lookup_intern_extern_type,
    name
);
define_language_element_id!(TraitId, TraitLongId, ast::ItemTrait, lookup_intern_trait, name);
define_language_element_id!(
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
define_language_element_id!(ImplId, ImplLongId, ast::ItemImpl, lookup_intern_impl, name);

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
    lookup_intern_generic_param,
    name
);
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
    /// Generic function ids enum.
    pub enum GenericFunctionId {
        Free(FreeFunctionId),
        Extern(ExternFunctionId),
        TraitFunction(TraitFunctionId),
        ImplFunction(ImplFunctionId),
    }
}
impl GenericFunctionId {
    pub fn format(&self, db: &(dyn DefsGroup + 'static)) -> String {
        format!("{}::{}", self.module(db).full_path(db), self.name(db))
    }
}

define_language_element_id_as_enum! {
    #[toplevel]
    /// Generic type ids enum.
    pub enum GenericTypeId {
        Struct(StructId),
        Enum(EnumId),
        Extern(ExternTypeId),
        // TODO(spapini): enums, associated types in impls.
    }
}
impl GenericTypeId {
    pub fn format(&self, db: &(dyn DefsGroup + 'static)) -> String {
        format!("{}::{}", self.module(db).full_path(db), self.name(db))
    }
}

/// Conversion from ModuleItemId to GenericFunctionId.
impl OptionFrom<ModuleItemId> for GenericFunctionId {
    fn option_from(item: ModuleItemId) -> Option<Self> {
        match item {
            ModuleItemId::FreeFunction(id) => Some(GenericFunctionId::Free(id)),
            ModuleItemId::ExternFunction(id) => Some(GenericFunctionId::Extern(id)),
            ModuleItemId::Submodule(_)
            | ModuleItemId::Use(_)
            | ModuleItemId::Trait(_)
            | ModuleItemId::Impl(_)
            | ModuleItemId::Struct(_)
            | ModuleItemId::Enum(_)
            | ModuleItemId::ExternType(_) => None,
        }
    }
}

/// Conversion from ModuleItemId to GenericTypeId.
impl OptionFrom<ModuleItemId> for GenericTypeId {
    fn option_from(item: ModuleItemId) -> Option<Self> {
        match item {
            ModuleItemId::Struct(id) => Some(GenericTypeId::Struct(id)),
            ModuleItemId::Enum(id) => Some(GenericTypeId::Enum(id)),
            ModuleItemId::ExternType(id) => Some(GenericTypeId::Extern(id)),
            ModuleItemId::Submodule(_)
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
