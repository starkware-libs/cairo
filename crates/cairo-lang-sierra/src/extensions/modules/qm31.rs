use num_traits::cast::ToPrimitive;

use super::is_zero::{IsZeroLibfunc, IsZeroTraits};
use super::non_zero::nonzero_ty;
use crate::define_libfunc_hierarchy;
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

/// Type for qm31.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct QM31Type {}
impl NoGenericArgsGenericType for QM31Type {
    const ID: GenericTypeId = GenericTypeId::new_inline("qm31");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

define_libfunc_hierarchy! {
    pub enum QM31Libfunc {
        BinaryOperation(QM31BinaryOperationLibfunc),
        Const(QM31ConstLibfunc),
        IsZero(QM31JumpNotZeroLibfunc),
    }, QM31Concrete
}

const M31_BOUND: i32 = i32::MAX - 1;

#[derive(Default)]
pub struct QM31Traits {}
impl IsZeroTraits for QM31Traits {
    const IS_ZERO: &'static str = "qm31_is_zero";
    const GENERIC_TYPE_ID: GenericTypeId = <QM31Type as NamedType>::ID;
}
pub type QM31JumpNotZeroLibfunc = IsZeroLibfunc<QM31Traits>;

/// QM31 binary operators.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum QM31BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

/// Libfunc for qm31 binary operations.
pub struct QM31BinaryOperationLibfunc {
    pub operator: QM31BinaryOperator,
}
impl QM31BinaryOperationLibfunc {
    fn new(operator: QM31BinaryOperator) -> Self {
        Self { operator }
    }
    const ADD: &'static str = "qm31_add";
    const SUB: &'static str = "qm31_sub";
    const MUL: &'static str = "qm31_mul";
    const DIV: &'static str = "qm31_div";
}
impl GenericLibfunc for QM31BinaryOperationLibfunc {
    type Concrete = QM31BinaryOpConcreteLibfunc;

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
            Self::ADD => Some(Self::new(QM31BinaryOperator::Add)),
            Self::SUB => Some(Self::new(QM31BinaryOperator::Sub)),
            Self::MUL => Some(Self::new(QM31BinaryOperator::Mul)),
            Self::DIV => Some(Self::new(QM31BinaryOperator::Div)),
            _ => None,
        }
    }

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(QM31Type::id(), &[])?;
        let second_param_type = if matches!(self.operator, QM31BinaryOperator::Div) {
            nonzero_ty(context, &ty)?
        } else {
            ty.clone()
        };
        match args {
            [] => Ok(LibfuncSignature::new_non_branch_ex(
                vec![
                    ParamSignature::new(ty.clone()),
                    ParamSignature::new(second_param_type).with_allow_const(),
                ],
                vec![OutputVarInfo { ty, ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 } }],
                SierraApChange::Known { new_vars_only: true },
            )),
            _ => Err(SpecializationError::WrongNumberOfGenericArgs),
        }
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [] => Ok({
                QM31BinaryOpConcreteLibfunc {
                    operator: self.operator,
                    signature: self.specialize_signature(context.upcast(), args)?,
                }
            }),
            _ => Err(SpecializationError::WrongNumberOfGenericArgs),
        }
    }
}

pub struct QM31BinaryOpConcreteLibfunc {
    pub operator: QM31BinaryOperator,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for QM31BinaryOpConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for creating a constant qm31.
#[derive(Default)]
pub struct QM31ConstLibfunc {}
impl NamedLibfunc for QM31ConstLibfunc {
    type Concrete = QM31ConstConcreteLibfunc;
    const STR_ID: &'static str = "qm31_const";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(QM31Type::id(), &[])?,
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
        let to_m31 = |arg: &GenericArg| -> Result<u32, SpecializationError> {
            match arg {
                GenericArg::Value(val) if *val <= M31_BOUND.into() => {
                    val.to_u32().ok_or(SpecializationError::UnsupportedGenericArg)
                }
                _ => Err(SpecializationError::UnsupportedGenericArg),
            }
        };
        match args {
            [w0, w1, w2, w3] => Ok(QM31ConstConcreteLibfunc {
                w0: to_m31(w0)?,
                w1: to_m31(w1)?,
                w2: to_m31(w2)?,
                w3: to_m31(w3)?,
                signature: <Self as NamedLibfunc>::specialize_signature(
                    self,
                    context.upcast(),
                    args,
                )?,
            }),
            _ => Err(SpecializationError::WrongNumberOfGenericArgs),
        }
    }
}

pub struct QM31ConstConcreteLibfunc {
    pub w0: u32,
    pub w1: u32,
    pub w2: u32,
    pub w3: u32,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for QM31ConstConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}
