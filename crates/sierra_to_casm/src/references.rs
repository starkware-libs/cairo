use std::collections::HashMap;

use casm::ap_change::{ApChange, ApChangeError, ApplyApChange};
use casm::operand::{CellRef, DerefOrImmediate, Register};
use num_bigint::BigInt;
use sierra::extensions::felt::FeltOperator;
use sierra::ids::{ConcreteTypeId, VarId};
use sierra::program::{Function, StatementIdx};
use thiserror::Error;
use utils::casts::usize_as_i16;

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
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(BinOpExpression {
            op: self.op,
            a: self.a.apply_ap_change(ap_change)?,
            b: self.b.apply_ap_change(ap_change)?,
        })
    }
}

/// The expression representing a cell in the Sierra intermediate memory.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CellExpression {
    AllocateSegment,
    Deref(CellRef),
    DoubleDeref(CellRef),
    IntoSingleCellRef(CellRef),
    Immediate(BigInt),
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
    /// If there is only onw cell in the ReferenceExpression returns the contained CellExpression.
    pub fn try_unpack_single(&self) -> Result<CellExpression, ReferencesError> {
        if let [cell_expr] = &self.cells[..] {
            Ok(cell_expr.clone())
        } else {
            Err(ReferencesError::InvalidReferenceTypeForArgument)
        }
    }
}

impl ApplyApChange for CellExpression {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(match self {
            CellExpression::AllocateSegment => CellExpression::AllocateSegment,
            CellExpression::Deref(operand) => {
                CellExpression::Deref(operand.apply_ap_change(ap_change)?)
            }
            CellExpression::DoubleDeref(operand) => {
                CellExpression::DoubleDeref(operand.apply_ap_change(ap_change)?)
            }
            CellExpression::IntoSingleCellRef(operand) => {
                CellExpression::IntoSingleCellRef(operand.apply_ap_change(ap_change)?)
            }
            CellExpression::Immediate(operand) => CellExpression::Immediate(operand),
            CellExpression::BinOp(operand) => {
                CellExpression::BinOp(operand.apply_ap_change(ap_change)?)
            }
        })
    }
}

impl ApplyApChange for ReferenceExpression {
    fn apply_ap_change(self, ap_change: ApChange) -> Result<Self, ApChangeError> {
        Ok(ReferenceExpression {
            cells: self
                .cells
                .into_iter()
                .map(|cell_expr| cell_expr.apply_ap_change(ap_change))
                .collect::<Result<Vec<_>, _>>()?,
        })
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
