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
use filesystem::ids::CrateId;
use smol_str::SmolStr;

/// Utility macro for defining ids.
/// Defines a long id representing some element by a parent element and a name.
/// Also defines a short id to be used for interning of the long id.
/// See the documentation of 'define_short_id' for more details.
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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModuleId {
    CrateRoot(CrateId),
    Submodule(SubmoduleId),
}
id_by_name_and_parent!(SubmoduleId, SubmoduleLongId, ModuleId);

/// Id for direct children of a module.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModuleItemId {
    FreeFunction(FreeFunctionId),
    Struct(StructId),
    ExternType(ExternTypeId),
    ExternFunction(ExternFunctionId),
}
id_by_name_and_parent!(FreeFunctionId, FreeFunctionLongId, ModuleId);
id_by_name_and_parent!(StructId, StructLongId, ModuleId);
id_by_name_and_parent!(ExternTypeId, ExternTypeLongId, ModuleId);
id_by_name_and_parent!(ExternFunctionId, ExternFunctionLongId, ModuleId);

// Struct items.
id_by_name_and_parent!(MemberId, MemberLongId, StructId);

/// Id for any variable definition.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum VarId {
    Param(ParamId),
    Local(LocalVarId),
    // TODO(spapini): Add var from pattern matching.
}

/// Id for anything that can directly contain a parameter.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ParamContainerId {
    FreeFunction(FreeFunctionId),
    ExternFunction(ExternFunctionId),
}
id_by_name_and_parent!(ParamId, ParamLongId, ParamContainerId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BlockLongId {
    /// The block is the entire body of a function.
    FunctionBody(FunctionWithBodyId),
    /// The block is an inner block.
    Inner {
        parent: BlockId,
        /// Index of subblock inside its containing block, among all direct subblocks.
        index: usize,
    },
}
define_short_id!(BlockId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LocalVarLongId {
    parent_block: BlockId,
    name: SmolStr,
    // TODO(spapini): Introduce another hierarchy when we support patterns instead of
    //   single identifier let.
    /// Index of local variables inside its containing block, among all direct local
    /// variable definitions with the same name.
    index: usize,
}
define_short_id!(LocalVarId);

/// Id for any function definition that also contains a body with code.
/// For example, 'extern func' is not included.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionWithBodyId {
    FreeFunction(FreeFunctionId),
    // TODO(spapini): impl functions
}

/// Id for anything that can be a "Go to definition" result.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Symbol {
    ModuleItem(ModuleItemId),
    Var(VarId),
}
