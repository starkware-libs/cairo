use num_bigint::BigInt;
use num_traits::Zero;

use super::jump_not_zero::{JumpNotZeroLibfunc, JumpNotZeroTraits};
use super::non_zero::NonZeroType;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    GenericLibfunc, NamedLibfunc, NamedType, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::{id_from_string, GenericLibfuncId, GenericTypeId};
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
    pub enum FeltLibfunc {
        BinaryOperation(FeltBinaryOperationLibfunc),
        Const(FeltConstLibfunc),
        JumpNotZero(FeltJumpNotZeroLibfunc),
    }, FeltConcrete
}

#[derive(Default)]
pub struct FeltTraits {}
impl JumpNotZeroTraits for FeltTraits {
    const JUMP_NOT_ZERO: &'static str = "felt_jump_nz";
    const GENERIC_TYPE_ID: GenericTypeId = <FeltType as NamedType>::ID;
}
pub type FeltJumpNotZeroLibfunc = JumpNotZeroLibfunc<FeltTraits>;

/// Felt binary operators.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FeltBinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

/// Libfunc for felt binary operations.
pub struct FeltBinaryOperationLibfunc {
    pub operator: FeltBinaryOperator,
}
impl FeltBinaryOperationLibfunc {
    fn new(operator: FeltBinaryOperator) -> Self {
        Self { operator }
    }
}
impl GenericLibfunc for FeltBinaryOperationLibfunc {
    type Concrete = FeltBinaryOperationConcreteLibfunc;

    fn by_id(id: &GenericLibfuncId) -> Option<Self> {
        const ADD: u64 = id_from_string("felt_add");
        const SUB: u64 = id_from_string("felt_sub");
        const MUL: u64 = id_from_string("felt_mul");
        const DIV: u64 = id_from_string("felt_div");
        match id.id {
            ADD => Some(Self::new(FeltBinaryOperator::Add)),
            SUB => Some(Self::new(FeltBinaryOperator::Sub)),
            MUL => Some(Self::new(FeltBinaryOperator::Mul)),
            DIV => Some(Self::new(FeltBinaryOperator::Div)),
            _ => None,
        }
    }

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(FeltType::id(), &[])?;
        match args {
            [] => Ok(LibfuncSignature::new_non_branch_ex(
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
                FeltBinaryOperationConcreteLibfunc::Binary(FeltBinaryOpConcreteLibfunc {
                    operator: self.operator,
                    signature: self.specialize_signature(context.upcast(), args)?,
                })
            }),
            [GenericArg::Value(c)] => {
                if matches!(self.operator, FeltBinaryOperator::Div) && c.is_zero() {
                    Err(SpecializationError::UnsupportedGenericArg)
                } else {
                    Ok(FeltBinaryOperationConcreteLibfunc::Const(
                        FeltOperationWithConstConcreteLibfunc {
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

pub struct FeltBinaryOpConcreteLibfunc {
    pub operator: FeltBinaryOperator,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for FeltBinaryOpConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Felt operations with a const.
pub struct FeltOperationWithConstConcreteLibfunc {
    pub operator: FeltBinaryOperator,
    pub c: BigInt,
    pub signature: LibfuncSignature,
}
define_concrete_libfunc_hierarchy! {
    pub enum FeltBinaryOperationConcreteLibfunc {
        Binary(FeltBinaryOpConcreteLibfunc),
        Const(FeltOperationWithConstConcreteLibfunc),
    }
}

impl SignatureBasedConcreteLibfunc for FeltOperationWithConstConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for creating a constant felt.
#[derive(Default)]
pub struct FeltConstLibfunc {}
impl NamedLibfunc for FeltConstLibfunc {
    type Concrete = FeltConstConcreteLibfunc;
    const STR_ID: &'static str = "felt_const";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
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
            [GenericArg::Value(c)] => Ok(FeltConstConcreteLibfunc {
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

pub struct FeltConstConcreteLibfunc {
    pub c: BigInt,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for FeltConstConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}
