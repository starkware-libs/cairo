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
// TODO(spapini): Add ModuleLongId once crates are added.
define_short_id!(ModuleId);

// ModuleItemId - direct children of a module.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModuleItemId {
    Submodule(SubmoduleId),
    Function(FunctionId),
    Struct(StructId),
    Enum(EnumId),
    Trait(TraitId),
    Impl(ImplId),
    Use(UseId),
    ExternType(ExternTypeId),
    ExternFunction(ExternFunctionId),
}
id_by_name_and_parent!(SubmoduleId, SubmoduleLongId, ModuleId);
id_by_name_and_parent!(FunctionId, FunctionLongId, ModuleId);
id_by_name_and_parent!(StructId, StructLongId, ModuleId);
id_by_name_and_parent!(EnumId, EnumLongId, ModuleId);
id_by_name_and_parent!(TraitId, TraitLongId, ModuleId);
id_by_name_and_parent!(ImplId, ImplLongId, ModuleId);
id_by_name_and_parent!(UseId, UseLongId, ModuleId);
id_by_name_and_parent!(ExternTypeId, ExternTypeLongId, ModuleId);
id_by_name_and_parent!(ExternFunctionId, ExternFunctionLongId, ModuleId);

// Struct items.
id_by_name_and_parent!(MemberId, MemberLongId, StructId);

// Enum items.
id_by_name_and_parent!(VariantId, VariantLongId, EnumId);

// TraitItemId - direct children of a trait.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum TraitItemId {
    AssociatedType(TraitAssociatedTypeId),
    Function(TraitFunctionId),
}
id_by_name_and_parent!(TraitAssociatedTypeId, TraitAssociatedTypeLongId, TraitId);
id_by_name_and_parent!(TraitFunctionId, TraitFunctionLongId, TraitId);

// ImplItemId - direct children of an impl.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ImplItemId {
    AssociatedType(ImplAssociatedTypeId),
    Function(ImplFunctionId),
}
id_by_name_and_parent!(ImplAssociatedTypeId, ImplAssociatedTypeLongId, ImplId);
id_by_name_and_parent!(ImplFunctionId, ImplFunctionLongId, ImplId);

// Generics.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericParamId {
    Type(GenericTypeParamId),
    Impl(GenericImplParamId),
}
id_by_name_and_parent!(GenericTypeParamId, GenericTypeParamLongId, ItemWithGenericsId);
id_by_name_and_parent!(GenericImplParamId, GenericImplParamLongId, ItemWithGenericsId);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ItemWithGenericsId {
    Function(FunctionId),
    Struct(StructId),
    Enum(EnumId),
    Trait(TraitId),
    TraitAssociatedType(TraitAssociatedTypeId),
    TraitFunction(TraitFunctionId),
    Impl(ImplId),
    ImplAssociatedType(ImplAssociatedTypeId),
    ImplFunction(ImplFunctionId),
    ExternType(ExternTypeId),
    ExternFunction(ExternFunctionId),
}

// Function parameters.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ItemWithFunctionSignatureId {
    Function(FunctionId),
    ImplFunction(ImplFunctionId),
    TraitFunction(TraitFunctionId),
    ExternFunction(ExternFunctionId),
}
id_by_name_and_parent!(FunctionParamId, FunctionParamLongId, ItemWithFunctionSignatureId);

// Definitions from code blocks.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum VarId {
    Param(FunctionParamId),
    Local(LetVarId),
    // TODO(spapini): Add var from pattern matching.
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BlockLongId {
    FunctionBody(ItemWithFunctionBodyId),
    Inner { parent: BlockId, index: usize },
}
define_short_id!(BlockId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LetVarLongId {
    block: BlockId,
    name: SmolStr,
    index: usize,
}
define_short_id!(LetVarId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ItemWithFunctionBodyId {
    Function(FunctionId),
    ImplFunction(ImplFunctionId),
    ExternFunction(ExternFunctionId),
}

// Symbol - anything that can be a "Go to definition" result.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Symbol {
    ModuleItem(ModuleItemId),
    TraitItem(TraitItemId),
    ImplItem(ImplItemId),
    GenericParam(GenericParamId),
    Var(VarId),
}
