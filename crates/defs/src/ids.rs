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
use filesystem::ids::CrateId;
use smol_str::SmolStr;
use syntax::node::helpers::GetIdentifier;
use syntax::node::{ast, TypedSyntaxNode};
use utils::OptionFrom;

use crate::db::DefsGroup;

// A trait for an id for a language element.
pub trait LanguageElementId {
    fn module(&self, db: &dyn DefsGroup) -> ModuleId;
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
/// See the documentation of 'define_short_id' and `stable_ptr.rs` for more details.
macro_rules! define_language_element_id {
    ($short_id:ident, $long_id:ident, $ast_ty:ty, $lookup:ident) => {
        #[derive(Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $long_id(pub ModuleId, pub <$ast_ty as TypedSyntaxNode>::StablePtr);
        impl $long_id {
            pub fn name(&self, db: &dyn DefsGroup) -> SmolStr {
                let syntax_db = db.upcast();
                let terminal_green = self.1.name_green(syntax_db);
                terminal_green.identifier(syntax_db)
            }
        }
        define_short_id!($short_id, $long_id, DefsGroup, $lookup);
        impl $short_id {
            pub fn stable_ptr(self, db: &dyn DefsGroup) -> <$ast_ty as TypedSyntaxNode>::StablePtr {
                db.$lookup(self).1
            }
        }
        impl<T: ?Sized + db_utils::Upcast<dyn DefsGroup + 'static>> debug::DebugWithDb<T>
            for $long_id
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &T) -> std::fmt::Result {
                let db: &(dyn DefsGroup + 'static) = db.upcast();
                let $long_id(module_id, _stable_ptr) = self;
                write!(
                    f,
                    "{}({}::{})",
                    stringify!($short_id),
                    module_id.full_path(db),
                    self.name(db)
                )
            }
        }
        impl LanguageElementId for $short_id {
            fn module(&self, db: &dyn DefsGroup) -> ModuleId {
                db.$lookup(*self).0
            }
            fn name(&self, db: &dyn DefsGroup) -> SmolStr {
                db.$lookup(*self).name(db)
            }
        }
    };
}

/// Defines and implements LanguageElementId for a subset of other language elements.
macro_rules! define_language_element_id_as_enum {
    (
        #[doc = $doc:expr]
        pub enum $enum_name:ident {
            $($variant:ident ($variant_ty:ty),)*
        }
    ) => {
        #[doc = $doc]
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
            fn name(&self, db: &dyn DefsGroup) -> SmolStr {
                match self {
                    $(
                        $enum_name::$variant(id) => id.name(db),
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
                format!("{}::{}", id.module(db).full_path(db), id.name(db))
            }
        }
    }
}
impl DebugWithDb<dyn DefsGroup> for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn DefsGroup) -> std::fmt::Result {
        write!(f, "ModuleId({})", self.full_path(db))
    }
}

define_language_element_id_as_enum! {
    /// Id for direct children of a module.
    pub enum ModuleItemId {
        Submodule(SubmoduleId),
        Use(UseId),
        FreeFunction(FreeFunctionId),
        Struct(StructId),
        ExternType(ExternTypeId),
        ExternFunction(ExternFunctionId),
    }
}
define_language_element_id!(SubmoduleId, SubmoduleLongId, ast::ItemModule, lookup_intern_submodule);
define_language_element_id!(UseId, UseLongId, ast::ItemUse, lookup_intern_use);
define_language_element_id!(
    FreeFunctionId,
    FreeFunctionLongId,
    ast::ItemFreeFunction,
    lookup_intern_free_function
);
define_language_element_id!(
    ExternFunctionId,
    ExternFunctionLongId,
    ast::ItemExternFunction,
    lookup_intern_extern_function
);
define_language_element_id!(StructId, StructLongId, ast::ItemStruct, lookup_intern_struct);
define_language_element_id!(
    ExternTypeId,
    ExternTypeLongId,
    ast::ItemExternType,
    lookup_intern_extern_type
);

// Struct items.
// TODO(spapini): Override full_path for to include parents, for better debug.
define_language_element_id!(MemberId, MemberLongId, ast::Param, lookup_intern_member);

define_language_element_id_as_enum! {
    /// Id for any variable definition.
    pub enum VarId {
        Param(ParamId),
        Local(LocalVarId),
        // TODO(spapini): Add var from pattern matching.
    }
}

// TODO(spapini): Override full_path for to include parents, for better debug.
define_language_element_id!(ParamId, ParamLongId, ast::Param, lookup_intern_param);
// TODO(spapini): change this to a binding inside a pattern.
// TODO(spapini): Override full_path for to include parents, for better debug.
define_language_element_id!(LocalVarId, LocalVarLongId, ast::StatementLet, lookup_intern_local_var);

define_language_element_id_as_enum! {
    /// Generic function ids enum.
    pub enum GenericFunctionId {
        Free(FreeFunctionId),
        Extern(ExternFunctionId),
        // TODO(spapini): impl functions.
    }
}

define_language_element_id_as_enum! {
    /// Generic type ids enum.
    pub enum GenericTypeId {
        Struct(StructId),
        Extern(ExternTypeId),
        // TODO(spapini): enums, associated types in impls.
    }
}
impl GenericTypeId {
    pub fn format(&self, db: &(dyn DefsGroup + 'static)) -> String {
        format!("{}::{}", self.module(db).full_path(db), self.name(db))
    }
}

/// Id for anything that can be a "Go to definition" result.
pub enum Symbol {
    Crate(CrateId),
    ModuleItem(ModuleItemId),
    Var(VarId),
}

impl OptionFrom<Symbol> for ModuleItemId {
    fn option_from(symbol: Symbol) -> Option<Self> {
        match symbol {
            Symbol::ModuleItem(item) => Some(item),
            _ => None,
        }
    }
}

// Conversion from ModuleItemId to GenericFunctionId.
impl OptionFrom<ModuleItemId> for GenericFunctionId {
    fn option_from(item: ModuleItemId) -> Option<Self> {
        match item {
            ModuleItemId::FreeFunction(id) => Some(GenericFunctionId::Free(id)),
            ModuleItemId::ExternFunction(id) => Some(GenericFunctionId::Extern(id)),
            ModuleItemId::Submodule(_)
            | ModuleItemId::Use(_)
            | ModuleItemId::Struct(_)
            | ModuleItemId::ExternType(_) => None,
        }
    }
}
impl OptionFrom<Symbol> for GenericFunctionId {
    fn option_from(symbol: Symbol) -> Option<Self> {
        ModuleItemId::option_from(symbol).and_then(GenericFunctionId::option_from)
    }
}
impl OptionFrom<ModuleItemId> for GenericTypeId {
    fn option_from(item: ModuleItemId) -> Option<Self> {
        match item {
            ModuleItemId::Struct(id) => Some(GenericTypeId::Struct(id)),
            ModuleItemId::ExternType(id) => Some(GenericTypeId::Extern(id)),
            ModuleItemId::Submodule(_)
            | ModuleItemId::Use(_)
            | ModuleItemId::FreeFunction(_)
            | ModuleItemId::ExternFunction(_) => None,
        }
    }
}
impl OptionFrom<Symbol> for GenericTypeId {
    fn option_from(symbol: Symbol) -> Option<Self> {
        ModuleItemId::option_from(symbol).and_then(GenericTypeId::option_from)
    }
}
