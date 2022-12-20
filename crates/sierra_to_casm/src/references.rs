use std::collections::HashMap;

use casm::ap_change::ApplyApChange;
use casm::operand::{BinOpOperand, CellRef, DerefOrImmediate, Register, ResOperand};
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use sierra::extensions::felt::{FeltBinaryOperator, FeltUnaryOperator};
use sierra::ids::{ConcreteTypeId, VarId};
use sierra::program::{Function, StatementIdx};
use thiserror::Error;
use utils::try_extract_matches;
use {casm, sierra};

use crate::invocations::InvocationError;
use crate::type_sizes::TypeSizeMap;

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
pub struct UnaryOpExpression {
    pub op: FeltUnaryOperator,
    pub a: DerefOrImmediate,
}
impl ApplyApChange for UnaryOpExpression {
    fn apply_known_ap_change(self, ap_change: usize) -> Option<Self> {
        Some(UnaryOpExpression { op: self.op, a: self.a.apply_known_ap_change(ap_change)? })
    }

    fn can_apply_unknown(&self) -> bool {
        self.a.can_apply_unknown()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinOpExpression {
    pub op: FeltBinaryOperator,
    pub a: CellRef,
    pub b: DerefOrImmediate,
}
impl ApplyApChange for BinOpExpression {
    fn apply_known_ap_change(self, ap_change: usize) -> Option<Self> {
        Some(BinOpExpression {
            op: self.op,
            a: self.a.apply_known_ap_change(ap_change)?,
            b: self.b.apply_known_ap_change(ap_change)?,
        })
    }

    fn can_apply_unknown(&self) -> bool {
        self.a.can_apply_unknown() && self.b.can_apply_unknown()
    }
}

/// The expression representing a cell in the Sierra intermediate memory.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CellExpression {
    Deref(CellRef),
    /// Represents an expression of the form `[[cell_ref] + offset]`.
    DoubleDeref(CellRef, i16),
    IntoSingleCellRef(CellRef),
    Immediate(BigInt),
    UnaryOp(UnaryOpExpression),
    BinOp(BinOpExpression),
}
impl CellExpression {
    pub fn from_res_operand(operand: ResOperand) -> Self {
        match operand {
            ResOperand::Deref(cell) => Self::Deref(cell),
            ResOperand::DoubleDeref(cell, offset) => Self::DoubleDeref(cell, offset),
            ResOperand::Immediate(imm) => Self::Immediate(imm),
            ResOperand::BinOp(op) => Self::BinOp(BinOpExpression {
                op: match op.op {
                    casm::operand::Operation::Add => FeltBinaryOperator::Add,
                    casm::operand::Operation::Mul => FeltBinaryOperator::Mul,
                },
                a: op.a,
                b: op.b,
            }),
        }
    }

    /// Extract the cell reference from the cell expression.
    pub fn to_deref(&self) -> Result<CellRef, InvocationError> {
        try_extract_matches!(self, CellExpression::Deref)
            .cloned()
            .ok_or(InvocationError::InvalidReferenceExpressionForArgument)
    }

    /// Given `[ref] + offset` returns `([ref], offset)`.
    pub fn to_deref_with_offset(&self) -> Result<(CellRef, i16), InvocationError> {
        match self {
            CellExpression::Deref(cell) => Ok((*cell, 0i16)),
            CellExpression::BinOp(BinOpExpression {
                op: FeltBinaryOperator::Add,
                a: cell,
                b: DerefOrImmediate::Immediate(offset),
            }) => Ok((
                *cell,
                offset.to_i16().ok_or(InvocationError::InvalidReferenceExpressionForArgument)?,
            )),
            _ => Err(InvocationError::InvalidReferenceExpressionForArgument),
        }
    }

    /// Returns the reference as a buffer with at least `required_slack` next cells that can be
    /// written as an instruction offset.
    pub fn to_buffer(&self, required_slack: i16) -> Result<ResOperand, InvocationError> {
        let (base, offset) = self.to_deref_with_offset()?;
        offset
            .checked_add(required_slack)
            .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;
        if offset == 0 {
            Ok(ResOperand::Deref(base))
        } else {
            Ok(ResOperand::BinOp(BinOpOperand {
                op: casm::operand::Operation::Add,
                a: base,
                b: DerefOrImmediate::Immediate(offset.into()),
            }))
        }
    }
}

/// A collection of Cell Expression which represents one logical object.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReferenceExpression {
    pub cells: Vec<CellExpression>,
}

impl ReferenceExpression {
    /// Builds a reference expression containing only a single cell
    pub fn from_cell(cell_expr: CellExpression) -> Self {
        Self { cells: vec![cell_expr] }
    }
    /// If there is only one cell in the ReferenceExpression returns the contained CellExpression.
    pub fn try_unpack_single(&self) -> Result<CellExpression, InvocationError> {
        if let [cell_expr] = &self.cells[..] {
            Ok(cell_expr.clone())
        } else {
            Err(InvocationError::InvalidReferenceExpressionForArgument)
        }
    }
}

impl ApplyApChange for CellExpression {
    fn apply_known_ap_change(self, ap_change: usize) -> Option<Self> {
        Some(match self {
            CellExpression::Deref(operand) => {
                CellExpression::Deref(operand.apply_known_ap_change(ap_change)?)
            }
            CellExpression::DoubleDeref(operand, offset) => {
                CellExpression::DoubleDeref(operand.apply_known_ap_change(ap_change)?, offset)
            }
            CellExpression::IntoSingleCellRef(operand) => {
                CellExpression::IntoSingleCellRef(operand.apply_known_ap_change(ap_change)?)
            }
            CellExpression::UnaryOp(operand) => {
                CellExpression::UnaryOp(operand.apply_known_ap_change(ap_change)?)
            }
            CellExpression::BinOp(operand) => {
                CellExpression::BinOp(operand.apply_known_ap_change(ap_change)?)
            }
            expr @ CellExpression::Immediate(_) => expr,
        })
    }

    fn can_apply_unknown(&self) -> bool {
        match self {
            CellExpression::Deref(operand)
            | CellExpression::DoubleDeref(operand, _)
            | CellExpression::IntoSingleCellRef(operand) => operand.can_apply_unknown(),
            CellExpression::Immediate(_) => true,
            CellExpression::UnaryOp(operand) => operand.can_apply_unknown(),
            CellExpression::BinOp(operand) => operand.can_apply_unknown(),
        }
    }
}

impl ApplyApChange for ReferenceExpression {
    fn apply_known_ap_change(self, ap_change: usize) -> Option<Self> {
        Some(ReferenceExpression {
            cells: self
                .cells
                .into_iter()
                .map(|cell_expr| cell_expr.apply_known_ap_change(ap_change))
                .collect::<Option<Vec<_>>>()?,
        })
    }

    fn can_apply_unknown(&self) -> bool {
        self.cells.iter().all(|cell| cell.can_apply_unknown())
    }
}

/// Builds the HashMap of references to the arguments of a function.
pub fn build_function_arguments_refs(
    func: &Function,
    type_sizes: &TypeSizeMap,
) -> Result<StatementRefs, ReferencesError> {
    let mut refs = HashMap::with_capacity(func.params.len());
    let mut offset = -3_i16;
    for param in func.params.iter().rev() {
        let size = type_sizes
            .get(&param.ty)
            .ok_or_else(|| ReferencesError::InvalidFunctionDeclaration(func.clone()))?;
        if refs
            .insert(
                param.id.clone(),
                ReferenceValue {
                    expression: ReferenceExpression {
                        cells: ((offset - size + 1)..(offset + 1))
                            .map(|i| {
                                CellExpression::Deref(CellRef { register: Register::FP, offset: i })
                            })
                            .collect(),
                    },
                    ty: param.ty.clone(),
                },
            )
            .is_some()
        {
            return Err(ReferencesError::InvalidFunctionDeclaration(func.clone()));
        }
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
