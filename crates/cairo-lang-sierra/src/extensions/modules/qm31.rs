use num_bigint::BigInt;
use num_traits::Zero;
use num_traits::cast::ToPrimitive;

use super::bounded_int::bounded_int_ty;
use super::is_zero::{IsZeroLibfunc, IsZeroTraits};
use super::non_zero::nonzero_ty;
use super::range_check::RangeCheckType;
use super::utils::reinterpret_cast_signature;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    GenericLibfunc, NamedLibfunc, NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType,
    OutputVarReferenceInfo, SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibfuncId, GenericTypeId};
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
        Pack(QM31PackLibfunc),
        Unpack(QM31UnpackLibfunc),
        FromM31(QM31FromM31Libfunc),
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

/// M31 value type.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum M31ValueType {
    /// A single M31 value.
    Single,
    /// A full QM31 type.
    Quad,
}

/// Libfunc for qm31 binary operations.
pub struct QM31BinaryOperationLibfunc {
    pub operator: QM31BinaryOperator,
    pub value_type: M31ValueType,
}
impl QM31BinaryOperationLibfunc {
    fn new(operator: QM31BinaryOperator, value_type: M31ValueType) -> Self {
        Self { operator, value_type }
    }
    const QM31_ADD: &'static str = "qm31_add";
    const QM31_SUB: &'static str = "qm31_sub";
    const QM31_MUL: &'static str = "qm31_mul";
    const QM31_DIV: &'static str = "qm31_div";
    const M31_ADD: &'static str = "m31_add";
    const M31_SUB: &'static str = "m31_sub";
    const M31_MUL: &'static str = "m31_mul";
    const M31_DIV: &'static str = "m31_div";
}
impl GenericLibfunc for QM31BinaryOperationLibfunc {
    type Concrete = QM31BinaryOpConcreteLibfunc;

    fn supported_ids() -> Vec<GenericLibfuncId> {
        vec![
            GenericLibfuncId::from(Self::QM31_ADD),
            GenericLibfuncId::from(Self::QM31_SUB),
            GenericLibfuncId::from(Self::QM31_MUL),
            GenericLibfuncId::from(Self::QM31_DIV),
            GenericLibfuncId::from(Self::M31_ADD),
            GenericLibfuncId::from(Self::M31_SUB),
            GenericLibfuncId::from(Self::M31_MUL),
            GenericLibfuncId::from(Self::M31_DIV),
        ]
    }

    fn by_id(id: &GenericLibfuncId) -> Option<Self> {
        match id.0.as_str() {
            Self::QM31_ADD => Some(Self::new(QM31BinaryOperator::Add, M31ValueType::Quad)),
            Self::QM31_SUB => Some(Self::new(QM31BinaryOperator::Sub, M31ValueType::Quad)),
            Self::QM31_MUL => Some(Self::new(QM31BinaryOperator::Mul, M31ValueType::Quad)),
            Self::QM31_DIV => Some(Self::new(QM31BinaryOperator::Div, M31ValueType::Quad)),
            Self::M31_ADD => Some(Self::new(QM31BinaryOperator::Add, M31ValueType::Single)),
            Self::M31_SUB => Some(Self::new(QM31BinaryOperator::Sub, M31ValueType::Single)),
            Self::M31_MUL => Some(Self::new(QM31BinaryOperator::Mul, M31ValueType::Single)),
            Self::M31_DIV => Some(Self::new(QM31BinaryOperator::Div, M31ValueType::Single)),
            _ => None,
        }
    }

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = match self.value_type {
            M31ValueType::Quad => context.get_concrete_type(QM31Type::id(), &[]),
            M31ValueType::Single => m31_ty(context),
        }?;
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
                    signature: self.specialize_signature(context, args)?,
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
                signature: <Self as NamedLibfunc>::specialize_signature(self, context, args)?,
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

/// Libfunc for packing 4 `m31`s into a `qm31`.
#[derive(Default)]
pub struct QM31PackLibfunc {}
impl NoGenericArgsGenericLibfunc for QM31PackLibfunc {
    const STR_ID: &'static str = "qm31_pack";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature::new(m31_ty(context)?); 4],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(QM31Type::id(), &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for unpacking a `qm31` into 4 `m31`s.
#[derive(Default)]
pub struct QM31UnpackLibfunc {}
impl NoGenericArgsGenericLibfunc for QM31UnpackLibfunc {
    const STR_ID: &'static str = "qm31_unpack";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let range_check_ty = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let output_var_info =
            OutputVarInfo { ty: m31_ty(context)?, ref_info: OutputVarReferenceInfo::SimpleDerefs };
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(range_check_ty.clone()).with_allow_add_const(),
                ParamSignature::new(context.get_concrete_type(QM31Type::id(), &[])?),
            ],
            vec![
                OutputVarInfo::new_builtin(range_check_ty, 0),
                output_var_info.clone(),
                output_var_info.clone(),
                output_var_info.clone(),
                output_var_info,
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for casting from an m31 to qm31.
#[derive(Default)]
pub struct QM31FromM31Libfunc {}
impl NoGenericArgsGenericLibfunc for QM31FromM31Libfunc {
    const STR_ID: &'static str = "qm31_from_m31";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(reinterpret_cast_signature(
            m31_ty(context)?,
            context.get_concrete_type(QM31Type::id(), &[])?,
        ))
    }
}

fn m31_ty(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    bounded_int_ty(context, BigInt::zero(), M31_BOUND.into())
}
