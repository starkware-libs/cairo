use num_bigint::BigInt;
use num_traits::Zero;

use super::jump_not_zero::{JumpNotZeroLibFunc, JumpNotZeroTraits};
use super::non_zero::NonZeroType;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    GenericLibFunc, NamedLibFunc, NamedType, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SignatureBasedConcreteLibFunc, SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;
use crate::{define_concrete_libfunc_hierarchy, define_libfunc_hierarchy};

/// Type for felt.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct FeltType {}
impl NoGenericArgsGenericType for FeltType {
    const ID: GenericTypeId = GenericTypeId::new_inline("felt");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 1;
}

define_libfunc_hierarchy! {
    pub enum FeltLibFunc {
        BinaryOperation(FeltBinaryOperationLibFunc),
        Const(FeltConstLibFunc),
        JumpNotZero(FeltJumpNotZeroLibFunc),
    }, FeltConcrete
}

#[derive(Default)]
pub struct FeltTraits {}
impl JumpNotZeroTraits for FeltTraits {
    const JUMP_NOT_ZERO: GenericLibFuncId = GenericLibFuncId::new_inline("felt_jump_nz");
    const GENERIC_TYPE_ID: GenericTypeId = <FeltType as NamedType>::ID;
}
pub type FeltJumpNotZeroLibFunc = JumpNotZeroLibFunc<FeltTraits>;

/// Felt binary operators.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FeltBinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

/// Libfunc for felt binary operations.
pub struct FeltBinaryOperationLibFunc {
    pub operator: FeltBinaryOperator,
}
impl FeltBinaryOperationLibFunc {
    fn new(operator: FeltBinaryOperator) -> Self {
        Self { operator }
    }
}
impl GenericLibFunc for FeltBinaryOperationLibFunc {
    type Concrete = FeltBinaryOperationConcreteLibFunc;

    fn by_id(id: &GenericLibFuncId) -> Option<Self> {
        const ADD: GenericLibFuncId = GenericLibFuncId::new_inline("felt_add");
        const SUB: GenericLibFuncId = GenericLibFuncId::new_inline("felt_sub");
        const MUL: GenericLibFuncId = GenericLibFuncId::new_inline("felt_mul");
        const DIV: GenericLibFuncId = GenericLibFuncId::new_inline("felt_div");
        match id {
            id if id == &ADD => Some(Self::new(FeltBinaryOperator::Add)),
            id if id == &SUB => Some(Self::new(FeltBinaryOperator::Sub)),
            id if id == &MUL => Some(Self::new(FeltBinaryOperator::Mul)),
            id if id == &DIV => Some(Self::new(FeltBinaryOperator::Div)),
            _ => None,
        }
    }

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(FeltType::id(), &[])?;
        match args {
            [] => Ok(LibFuncSignature::new_non_branch_ex(
                vec![
                    ParamSignature::new(ty.clone()),
                    ParamSignature {
                        ty: if matches!(self.operator, FeltBinaryOperator::Div) {
                            context.get_wrapped_concrete_type(NonZeroType::id(), ty.clone())?
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
                if matches!(self.operator, FeltBinaryOperator::Div) && c.is_zero() {
                    Err(SpecializationError::UnsupportedGenericArg)
                } else {
                    Ok(LibFuncSignature::new_non_branch(
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
                FeltBinaryOperationConcreteLibFunc::Binary(FeltBinaryOpConcreteLibFunc {
                    operator: self.operator,
                    signature: self.specialize_signature(context.upcast(), args)?,
                })
            }),
            [GenericArg::Value(c)] => {
                if matches!(self.operator, FeltBinaryOperator::Div) && c.is_zero() {
                    Err(SpecializationError::UnsupportedGenericArg)
                } else {
                    Ok(FeltBinaryOperationConcreteLibFunc::Const(
                        FeltOperationWithConstConcreteLibFunc {
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

pub struct FeltBinaryOpConcreteLibFunc {
    pub operator: FeltBinaryOperator,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for FeltBinaryOpConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}

/// Felt operations with a const.
pub struct FeltOperationWithConstConcreteLibFunc {
    pub operator: FeltBinaryOperator,
    pub c: BigInt,
    pub signature: LibFuncSignature,
}
define_concrete_libfunc_hierarchy! {
    pub enum FeltBinaryOperationConcreteLibFunc {
        Binary(FeltBinaryOpConcreteLibFunc),
        Const(FeltOperationWithConstConcreteLibFunc),
    }
}

impl SignatureBasedConcreteLibFunc for FeltOperationWithConstConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}

/// LibFunc for creating a constant felt.
#[derive(Default)]
pub struct FeltConstLibFunc {}
impl NamedLibFunc for FeltConstLibFunc {
    type Concrete = FeltConstConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("felt_const");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(FeltType::id(), &[])?,
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
            [GenericArg::Value(c)] => Ok(FeltConstConcreteLibFunc {
                c: c.clone(),
                signature: <Self as NamedLibFunc>::specialize_signature(
                    self,
                    context.upcast(),
                    args,
                )?,
            }),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct FeltConstConcreteLibFunc {
    pub c: BigInt,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for FeltConstConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}
