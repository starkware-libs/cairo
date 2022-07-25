use db_utils::define_short_id;
use defs::ids::{
    EnumId, ExternFunctionId, ExternTypeId, FunctionId, GenericImplArgId, GenericTypeArgId,
    ImplAssociatedTypeId, ImplFunctionId, ImplId, StructId,
};

// Ids in this file represent semantic representations.

// Generics.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericValueId {
    Type(TypeInstanceId),
    Impl(ImplInstanceId),
}

// Function instance.
// For example: ImplA::foo<A, B>, or bar<A>.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionInstanceLongId {
    kind: FunctionInstanceKind,
    generic_values: Vec<GenericValueId>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionInstanceKind {
    Top(FunctionId),
    Impl(ImplInstanceId, ImplFunctionId),
    Extern(ExternFunctionId),
}
define_short_id!(FunctionInstanceId);

// Impl instance.
// For example: ImplA<A, B>.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ImplInstanceLongId {
    Top(TopImplInstance),
    Generic(GenericImplArgId),
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TopImplInstance {
    imp: ImplId,
    generic_values: Vec<GenericValueId>,
}
define_short_id!(ImplInstanceId);

// Type instance.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeDefId {
    Generic(GenericTypeArgId),
    Struct(StructId),
    Enum(EnumId),
    External(ExternTypeId),
    Associated(ImplInstanceId, ImplAssociatedTypeId),
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeInstanceLongId {
    TypeDef(TypeDefId, Vec<GenericValueId>),
    Tuple(Vec<TypeInstanceId>),
    Missing,
}
define_short_id!(TypeInstanceId);

// CodeElements.
define_short_id!(ExprId);
define_short_id!(StatementId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CodeElementId {
    Statement(StatementId),
    Expr(ExprId),
}
