// The following ids represent all the definitions in the code.
// Roughly, every first appearance of an identifier.
// Everything that can be returned by "Go to definition" is a definition.
// Examples:
// * let x = 5.
//   Has a single definition for the variable "x".
// * func foo<T>(a: felt){return ();}.
//   Has 3 definitions:
//   * Function "foo".
//   * Generic argument "T".
//   * Function argument "a".
// * trait MyTrait{ func foo() -> (); }
//   Has 2 definitions:
//   * Trait "MyTrait"
//   * TraitFunction "foo".
// * impl A for MyTrait{ func foo() -> (){...} }
//   Has 2 definitions:
//   * Impl "A"
//   * ImplFunction "foo".
//
// Call sites, variable usages, assignments, etc.. are NOT definitions.

use db_utils::intern_id;
use smol_str::SmolStr;

// Utility macro for defining ids.
// Defines short id and a long id repreented by a parent and a name.
macro_rules! id_by_name_and_parent {
    ($id:ident, $long_id:ident, $parent_ty:ty) => {
        #[derive(Clone, Debug, Hash, PartialEq, Eq)]
        pub struct $long_id {
            pub parent: $parent_ty,
            pub name: SmolStr,
        }
        intern_id!($id);
    };
}

// Module.
// TODO: Add ModuleLongId once crates are added.
intern_id!(ModuleId);

// Module Items.
// ModuleItemId - direct children of a module.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModuleItemId {
    ModuleDef(ModuleDefId),
    TopFunction(TopFunctionId),
    Struct(StructId),
    Enum(EnumId),
    Trait(TraitId),
    Impl(ImplId),
    Use(UseId),
    ExternFunction(ExternFunctionId),
    ExternType(ExternTypeId),
}
id_by_name_and_parent!(ModuleDefId, ModuleDefLongId, ModuleId);
id_by_name_and_parent!(TopFunctionId, TopFunctionLongId, ModuleId);
id_by_name_and_parent!(StructId, StructLongId, ModuleId);
id_by_name_and_parent!(MemberId, MemberLongId, StructId);
id_by_name_and_parent!(EnumId, EnumLongId, ModuleId);
id_by_name_and_parent!(VariantId, VariantLongId, EnumId);
id_by_name_and_parent!(TraitId, TraitLongId, ModuleId);
id_by_name_and_parent!(ImplId, ImplLongId, ModuleId);
id_by_name_and_parent!(UseId, UseLongId, ModuleId);
id_by_name_and_parent!(ExternTypeId, ExternTypeLongId, ModuleId);
id_by_name_and_parent!(ExternFunctionId, ExternFunctionLongId, ModuleId);

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
pub enum GenericArgId {
    Type(GenericTypeArgId),
    Impl(GenericImplArgId),
}
id_by_name_and_parent!(GenericTypeArgId, GenericTypeArgLongId, ItemWithGenericsId);
id_by_name_and_parent!(GenericImplArgId, GenericImplArgLongId, ItemWithGenericsId);
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ItemWithGenericsId {
    TopFunction(TopFunctionId),
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

// Function args.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionId {
    // TODO: rename.
    TopFunction(TopFunctionId),
    ImplFunction(ImplFunctionId),
    ExternFunction(ExternFunctionId),
}
id_by_name_and_parent!(FunctionArgId, FunctionArgLongId, FunctionId);

// Local definitions.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum VarId {
    Arg(FunctionArgId),
    Local(LetVarId),
    // TODO: Add var from pattern matching.
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BlockLongId {
    FunctionBody(FunctionId),
    Inner { parent: BlockId, index: usize },
}
intern_id!(BlockId);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LetVarLongId {
    block: BlockId,
    name: SmolStr,
    index: usize,
}
intern_id!(LetVarId);

// Symbol - anything that can be a "Go to definition" result.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Symbol {
    ModuleItem(ModuleItemId),
    TraitItem(TraitItemId),
    ImplItem(ImplItemId),
    GenericArg(GenericArgId),
    Var(VarId),
}
