use std::collections::HashMap;

use casm::ap_change::ApplyApChange;
use casm::operand::{CellRef, DerefOrImmediate, Register};
use num_bigint::BigInt;
use sierra::extensions::felt::FeltOperator;
use sierra::ids::{ConcreteTypeId, VarId};
use sierra::program::{Function, StatementIdx};
use thiserror::Error;
use utils::casts::usize_as_i16;
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
pub struct BinOpExpression {
    pub op: FeltOperator,
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
    /// Represents an expression of the form [[cell_ref] + offset].
    DoubleDeref(CellRef, i16),
    IntoSingleCellRef(CellRef),
    Immediate(BigInt),
    Padding,
    BinOp(BinOpExpression),
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
    pub fn try_unpack_single(&self) -> Result<CellExpression, ReferencesError> {
        if let [cell_expr] = &self.cells[..] {
            Ok(cell_expr.clone())
        } else {
            Err(ReferencesError::InvalidReferenceTypeForArgument)
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
            CellExpression::BinOp(operand) => {
                CellExpression::BinOp(operand.apply_known_ap_change(ap_change)?)
            }
            expr @ (CellExpression::Padding | CellExpression::Immediate(_)) => expr,
        })
    }

    fn can_apply_unknown(&self) -> bool {
        match self {
            CellExpression::Deref(operand)
            | CellExpression::DoubleDeref(operand, _)
            | CellExpression::IntoSingleCellRef(operand) => operand.can_apply_unknown(),
            CellExpression::Immediate(_) | CellExpression::Padding => true,
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
                        cells: ((offset - usize_as_i16(*size) + 1)..(offset + 1))
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
        offset -= usize_as_i16(*size);
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

/// Extract the cell reference from the reference expression.
pub fn try_unpack_deref(expr: &ReferenceExpression) -> Result<CellRef, InvocationError> {
    expr.try_unpack_single()
        .ok()
        .and_then(|cell| try_extract_matches!(cell, CellExpression::Deref))
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)
}
