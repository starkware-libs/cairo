use std::collections::HashMap;

use casm::ap_change::{ApChange, ApChangeError, ApplyApChange};
use casm::operand::{
    DerefOperand, DerefOrImmediate, DoubleDerefOperand, ImmediateOperand, Register,
};
use sierra::extensions::arithmetic::Operator;
use sierra::ids::{ConcreteTypeId, VarId};
use sierra::program::{Function, StatementIdx};
use thiserror::Error;
use utils::try_extract_matches;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum ReferencesError {
    #[error("Invalid function declaration.")]
    InvalidFunctionDeclaration(Function),
    #[error("{var_id} is dangling at #{statement_idx}.")]
    DanglingReferences { statement_idx: StatementIdx, var_id: VarId },
    #[error(
        "One of the arguments does not match the expected type of the libfunc or return statement."
    )]
    InvalidReferenceTypeForArgument,
}

pub type StatementRefs = HashMap<VarId, ReferenceValue>;

/// A Sierra reference to a value.
/// Corresponds to an argument or return value of a Sierra statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReferenceValue {
    pub expression: ReferenceExpression,
    pub ty: ConcreteTypeId,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinOpExpression {
    pub op: Operator,
    pub a: DerefOperand,
    pub b: DerefOrImmediate,
}
impl ApplyApChange for BinOpExpression {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(BinOpExpression {
            op: self.op,
            a: self.a.apply_ap_change(ap_change)?,
            b: self.b.apply_ap_change(ap_change)?,
        })
    }
}

/// A struct representing an actual enum value in the Sierra program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumValue {
    // TODO(yuval): this is a bit of a hack. We might want to have 2 structs: EnumValue (with an
    // integer as variant_selector), StoredEnumValue (with a ::Deref as variant_selector), with
    // store_* translating EnumValue to StoredEnumValue.
    /// This must be ReferenceExpression::Immediate after enum_init, and must be
    /// ReferenceExpression::Deref after store_*.
    pub variant_selector: ReferenceExpression,
    /// The inner value of the enum - can be any ReferenceExpression.
    pub inner_value: ReferenceExpression,
}
impl TryFrom<&ReferenceExpression> for EnumValue {
    type Error = ReferencesError;

    fn try_from(expr: &ReferenceExpression) -> Result<Self, Self::Error> {
        let complex = try_extract_matches!(expr, ReferenceExpression::Complex)
            .ok_or(ReferencesError::InvalidReferenceTypeForArgument)?;
        if complex.len() != 2 {
            return Err(ReferencesError::InvalidReferenceTypeForArgument);
        }
        // TODO(yg): are boxes needed?
        Ok(EnumValue { variant_selector: complex[0].clone(), inner_value: complex[1].clone() })
    }
}
impl From<EnumValue> for ReferenceExpression {
    // fn into(self) -> ReferenceExpression {
    //     ReferenceExpression::Complex(vec![self.variant_selector, self.inner_value])
    // }

    fn from(enum_val: EnumValue) -> Self {
        ReferenceExpression::Complex(vec![enum_val.variant_selector, enum_val.inner_value])
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ReferenceExpression {
    AllocateSegment,
    Deref(DerefOperand),
    DoubleDeref(DoubleDerefOperand),
    IntoSingleCellRef(DerefOperand),
    Immediate(ImmediateOperand),
    BinOp(BinOpExpression),
    Complex(Vec<ReferenceExpression>),
}

impl ApplyApChange for ReferenceExpression {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(match self {
            ReferenceExpression::AllocateSegment => ReferenceExpression::AllocateSegment,
            ReferenceExpression::Deref(operand) => {
                ReferenceExpression::Deref(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::DoubleDeref(operand) => {
                ReferenceExpression::DoubleDeref(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::IntoSingleCellRef(operand) => {
                ReferenceExpression::IntoSingleCellRef(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::Immediate(operand) => {
                ReferenceExpression::Immediate(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::BinOp(operand) => {
                ReferenceExpression::BinOp(operand.apply_ap_change(ap_change)?)
            }
            ReferenceExpression::Complex(elements) => {
                let res_vec: Result<Vec<ReferenceExpression>, ApChangeError> = elements
                    .into_iter()
                    .map(|ref_expr| ref_expr.apply_ap_change(ap_change))
                    .collect();
                ReferenceExpression::Complex(res_vec?)
            }
        })
    }
}

/// Builds the HashMap of references to the parameters of a function.
pub fn build_function_parameter_refs(func: &Function) -> Result<StatementRefs, ReferencesError> {
    let mut refs = HashMap::with_capacity(func.params.len());

    let mut offset = -3;
    for param in func.params.iter().rev() {
        if refs
            .insert(
                param.id.clone(),
                ReferenceValue {
                    expression: ReferenceExpression::Deref(DerefOperand {
                        register: Register::FP,
                        offset,
                    }),
                    ty: param.ty.clone(),
                },
            )
            .is_some()
        {
            return Err(ReferencesError::InvalidFunctionDeclaration(func.clone()));
        }
        // TODO(ilya, 10/10/2022): Get size from type.
        let size = 1;
        offset -= size;
    }

    Ok(refs)
}

/// Checks that the list of references contains types matching the given types.
pub fn check_types_match(
    refs: &[ReferenceValue],
    types: &[ConcreteTypeId],
) -> Result<(), ReferencesError> {
    if itertools::equal(types.iter(), refs.iter().map(|r| &r.ty)) {
        Ok(())
    } else {
        Err(ReferencesError::InvalidReferenceTypeForArgument)
    }
}
