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
use filesystem::ids::ModuleId;
use smol_str::SmolStr;
use syntax::node::{ast, SyntaxNode, TypedSyntaxNode};

use crate::db::DefsGroup;

/// Utility macro for defining ids.
/// Defines a long id representing some element by a module_id and a stable pointer.
/// Also defines a short id to be used for interning of the long id.
/// Also requires the lookup function name for the lookup fo the long id from the short id,
/// as defined in DefsGroup.
/// See the documentation of 'define_short_id' and `stable_ptr.rs` for more details.
macro_rules! id_by_stable_ptr {
    ($short_id:ident, $long_id:ident, $ast_ty:ty, $lookup:ident) => {
        #[derive(Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $long_id(pub ModuleId, pub <$ast_ty as TypedSyntaxNode>::StablePtr);
        define_short_id!($short_id);
        impl $short_id {
            pub fn module(self, db: &dyn DefsGroup) -> ModuleId {
                db.$lookup(self).0
            }
            pub fn stable_ptr(self, db: &dyn DefsGroup) -> <$ast_ty as TypedSyntaxNode>::StablePtr {
                db.$lookup(self).1
            }
        }
    };
}
/// A trait for ids that have a "name" getter
pub trait HasName {
    fn name(self, db: &dyn DefsGroup) -> SmolStr;
}
/// Implements HasName for ids whose stable pointer has "name" as a key field.
/// See `stable_ptr.rs` for more details.
macro_rules! has_name_key_field {
    ($short_id:ident, $lookup:ident) => {
        impl HasName for $short_id {
            fn name(self, db: &dyn DefsGroup) -> SmolStr {
                let syntax_db = db.as_syntax_group();
                let terminal_green = db.$lookup(self).1.name_green(syntax_db);
                let terminal_ast = ast::Terminal::from_syntax_node(
                    syntax_db,
                    SyntaxNode::new_root(syntax_db, terminal_green),
                );
                terminal_ast.text(syntax_db)
            }
        }
    };
}

/// Id for direct children of a module.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModuleItemId {
    FreeFunction(FreeFunctionId),
    Struct(StructId),
    ExternType(ExternTypeId),
    ExternFunction(ExternFunctionId),
}
id_by_stable_ptr!(
    FreeFunctionId,
    FreeFunctionLongId,
    ast::ItemFreeFunction,
    lookup_intern_free_function
);
id_by_stable_ptr!(
    ExternFunctionId,
    ExternFunctionLongId,
    ast::ItemExternFunction,
    lookup_intern_extern_function
);
id_by_stable_ptr!(StructId, StructLongId, ast::ItemStruct, lookup_intern_struct);
id_by_stable_ptr!(ExternTypeId, ExternTypeLongId, ast::ItemExternType, lookup_intern_extern_type);
has_name_key_field!(FreeFunctionId, lookup_intern_free_function);
has_name_key_field!(ExternFunctionId, lookup_intern_extern_function);
has_name_key_field!(StructId, lookup_intern_struct);
has_name_key_field!(ExternTypeId, lookup_intern_extern_type);

// Struct items.
id_by_stable_ptr!(MemberId, MemberLongId, ast::Param, lookup_intern_member);
has_name_key_field!(MemberId, lookup_intern_member);

/// Id for any variable definition.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum VarId {
    Param(ParamId),
    Local(LocalVarId),
    // TODO(spapini): Add var from pattern matching.
}
impl VarId {
    pub fn name(&self, db: &dyn DefsGroup) -> SmolStr {
        match self {
            VarId::Param(id) => id.name(db),
            VarId::Local(id) => id.name(db),
        }
    }
}

/// Id for anything that can directly contain a parameter.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ParamContainerId {
    FreeFunction(FreeFunctionId),
    ExternFunction(ExternFunctionId),
}
impl ParamContainerId {
    pub fn module(&self, db: &dyn DefsGroup) -> ModuleId {
        match *self {
            ParamContainerId::FreeFunction(id) => db.lookup_intern_free_function(id).0,
            ParamContainerId::ExternFunction(id) => db.lookup_intern_extern_function(id).0,
        }
    }
}
id_by_stable_ptr!(ParamId, ParamLongId, ast::Param, lookup_intern_param);
has_name_key_field!(ParamId, lookup_intern_param);
id_by_stable_ptr!(BlockId, BlockLongId, ast::ExprBlock, lookup_intern_block);
// TODO(spapini): change this to a binding inside a pattern.
id_by_stable_ptr!(LocalVarId, LocalVarLongId, ast::StatementLet, lookup_intern_local_var);
has_name_key_field!(LocalVarId, lookup_intern_local_var);

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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericFunctionId {
    Free(FreeFunctionId),
    Extern(ExternFunctionId),
    // TODO(spapini): impl functions.
}
impl GenericFunctionId {
    pub fn module(&self, db: &dyn DefsGroup) -> ModuleId {
        match self {
            GenericFunctionId::Free(item) => item.module(db),
            GenericFunctionId::Extern(item) => item.module(db),
        }
    }
}

/// Type instance.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericTypeId {
    Struct(StructId),
    Extern(ExternTypeId),
    // TODO(spapini): enums, associated types in impls.
}
