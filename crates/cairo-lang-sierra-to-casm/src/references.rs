use std::collections::HashMap;

use cairo_lang_casm::ap_change::ApplyApChange;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::{CellRef, DerefOrImmediate, Register};
use cairo_lang_sierra::extensions::felt::FeltBinaryOperator;
use cairo_lang_sierra::ids::{ConcreteTypeId, VarId};
use cairo_lang_sierra::program::Function;
use thiserror::Error;
use {cairo_lang_casm, cairo_lang_sierra};

use crate::invocations::InvocationError;
use crate::type_sizes::TypeSizeMap;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum ReferencesError {
    #[error("Invalid function declaration.")]
    InvalidFunctionDeclaration(Function),
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

    /// If returns the cells as an array of the requested size if the size is correct.
    pub fn try_unpack<const SIZE: usize>(
        &self,
    ) -> Result<&[CellExpression; SIZE], InvocationError> {
        <&[CellExpression; SIZE]>::try_from(&self.cells[..])
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)
    }

    /// If there is only one cell in the ReferenceExpression returns the contained CellExpression.
    pub fn try_unpack_single(&self) -> Result<&CellExpression, InvocationError> {
        Ok(&self.try_unpack::<1>()?[0])
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
