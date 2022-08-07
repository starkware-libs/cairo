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
//   * Generic argument "T" (only the first occurrence of "T").
//   * Function argument "a".
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
use filesystem::ids::CrateId;
use smol_str::SmolStr;

// Utility macro for defining ids.
// Defines a long id representing some element by a parent element and a name.
// Also defines a short id to be used for interning of the long id.
// See the documentation of 'define_short_id' for more details.
macro_rules! id_by_name_and_parent {
    ($id:ident, $long_id:ident, $parent_ty:ty) => {
        #[derive(Clone, Debug, Hash, PartialEq, Eq)]
        pub struct $long_id {
            pub parent: $parent_ty,
            pub name: SmolStr,
        }
        define_short_id!($id);
    };
}

// Module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModuleId {
    CrateRoot(CrateId),
    Submodule(SubmoduleId),
}

// Module Items.
// ModuleItemId - direct children of a module.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModuleItemId {
    Submodule(SubmoduleId),
    Struct(StructId),
}
id_by_name_and_parent!(StructId, StructLongId, ModuleId);
id_by_name_and_parent!(SubmoduleId, SubmoduleLongId, ModuleId);

// Symbol - anything that can be a "Go to definition" result.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Symbol {
    ModuleItem(ModuleItemId),
}
