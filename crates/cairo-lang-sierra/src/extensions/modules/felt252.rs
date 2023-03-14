use num_bigint::BigInt;
use num_traits::Zero;

use super::is_zero::{IsZeroLibfunc, IsZeroTraits};
use super::non_zero::nonzero_ty;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    GenericLibfunc, NamedLibfunc, NamedType, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::{GenericLibfuncId, GenericTypeId};
use crate::program::GenericArg;
use crate::{define_concrete_libfunc_hierarchy, define_libfunc_hierarchy};

/// Type for felt252.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct Felt252Type {}
impl NoGenericArgsGenericType for Felt252Type {
    const ID: GenericTypeId = GenericTypeId::new_inline("felt252");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 1;
}

define_libfunc_hierarchy! {
    pub enum Felt252Libfunc {
        BinaryOperation(Felt252BinaryOperationLibfunc),
        Const(Felt252ConstLibfunc),
        IsZero(Felt252JumpNotZeroLibfunc),
    }, Felt252Concrete
}

#[derive(Default)]
pub struct Felt252Traits {}
impl IsZeroTraits for Felt252Traits {
    const IS_ZERO: &'static str = "felt252_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <Felt252Type as NamedType>::ID;
}
pub type Felt252JumpNotZeroLibfunc = IsZeroLibfunc<Felt252Traits>;

/// Felt252 binary operators.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Felt252BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

/// Libfunc for felt252 binary operations.
pub struct Felt252BinaryOperationLibfunc {
    pub operator: Felt252BinaryOperator,
}
impl Felt252BinaryOperationLibfunc {
    fn new(operator: Felt252BinaryOperator) -> Self {
        Self { operator }
    }
    const ADD: &str = "felt252_add";
    const SUB: &str = "felt252_sub";
    const MUL: &str = "felt252_mul";
    const DIV: &str = "felt252_div";
}
impl GenericLibfunc for Felt252BinaryOperationLibfunc {
    type Concrete = Felt252BinaryOperationConcreteLibfunc;

    fn supported_ids() -> Vec<GenericLibfuncId> {
        vec![
            GenericLibfuncId::from(Self::ADD),
            GenericLibfuncId::from(Self::SUB),
            GenericLibfuncId::from(Self::MUL),
            GenericLibfuncId::from(Self::DIV),
        ]
    }

    fn by_id(id: &GenericLibfuncId) -> Option<Self> {
        match id.0.as_str() {
            Self::ADD => Some(Self::new(Felt252BinaryOperator::Add)),
            Self::SUB => Some(Self::new(Felt252BinaryOperator::Sub)),
            Self::MUL => Some(Self::new(Felt252BinaryOperator::Mul)),
            Self::DIV => Some(Self::new(Felt252BinaryOperator::Div)),
            _ => None,
        }
    }

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        match args {
            [] => Ok(LibfuncSignature::new_non_branch_ex(
                vec![
                    ParamSignature::new(ty.clone()),
                    ParamSignature {
                        ty: if matches!(self.operator, Felt252BinaryOperator::Div) {
                            nonzero_ty(context, &ty)?
                        } else {
                            ty.clone()
                        },
                        allow_deferred: false,
                        allow_add_const: false,
                        allow_const: true,
                    },
                ],
                vec![OutputVarInfo {
                    ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                }],
                SierraApChange::Known { new_vars_only: true },
            )),
            [GenericArg::Value(c)] => {
                if matches!(self.operator, Felt252BinaryOperator::Div) && c.is_zero() {
                    Err(SpecializationError::UnsupportedGenericArg)
                } else {
                    Ok(LibfuncSignature::new_non_branch(
                        vec![ty.clone()],
                        vec![OutputVarInfo {
                            ty,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        }],
                        SierraApChange::Known { new_vars_only: true },
                    ))
                }
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [] => Ok({
                Felt252BinaryOperationConcreteLibfunc::Binary(Felt252BinaryOpConcreteLibfunc {
                    operator: self.operator,
                    signature: self.specialize_signature(context.upcast(), args)?,
                })
            }),
            [GenericArg::Value(c)] => {
                if matches!(self.operator, Felt252BinaryOperator::Div) && c.is_zero() {
                    Err(SpecializationError::UnsupportedGenericArg)
                } else {
                    Ok(Felt252BinaryOperationConcreteLibfunc::Const(
                        Felt252OperationWithConstConcreteLibfunc {
                            operator: self.operator,
                            c: c.clone(),
                            signature: self.specialize_signature(context.upcast(), args)?,
                        },
                    ))
                }
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct Felt252BinaryOpConcreteLibfunc {
    pub operator: Felt252BinaryOperator,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for Felt252BinaryOpConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Felt252 operations with a const.
pub struct Felt252OperationWithConstConcreteLibfunc {
    pub operator: Felt252BinaryOperator,
    pub c: BigInt,
    pub signature: LibfuncSignature,
}
define_concrete_libfunc_hierarchy! {
    pub enum Felt252BinaryOperationConcreteLibfunc {
        Binary(Felt252BinaryOpConcreteLibfunc),
        Const(Felt252OperationWithConstConcreteLibfunc),
    }
}

impl SignatureBasedConcreteLibfunc for Felt252OperationWithConstConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for creating a constant felt252.
#[derive(Default)]
pub struct Felt252ConstLibfunc {}
impl NamedLibfunc for Felt252ConstLibfunc {
    type Concrete = Felt252ConstConcreteLibfunc;
    const STR_ID: &'static str = "felt252_const";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(Felt252Type::id(), &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Const),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => Ok(Felt252ConstConcreteLibfunc {
                c: c.clone(),
                signature: <Self as NamedLibfunc>::specialize_signature(
                    self,
                    context.upcast(),
                    args,
                )?,
            }),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct Felt252ConstConcreteLibfunc {
    pub c: BigInt,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for Felt252ConstConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}
