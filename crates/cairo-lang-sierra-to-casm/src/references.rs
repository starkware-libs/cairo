use std::collections::HashMap;

use cairo_lang_casm::ap_change::ApplyApChange;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_sierra::ids::{ConcreteTypeId, VarId};
use cairo_lang_sierra::program::{Function, StatementIdx};
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
    #[error("Unknown type size for type id `{0}`.")]
    UnknownTypeSize(ConcreteTypeId),
}

pub type StatementRefs = HashMap<VarId, ReferenceValue>;

/// A Sierra reference to a value.
/// Corresponds to an argument or return value of a Sierra statement.
#[derive(Clone, Debug)]
pub struct ReferenceValue {
    pub expression: ReferenceExpression,
    pub ty: ConcreteTypeId,
    /// The index of the variable on the continuous-stack.
    pub stack_idx: Option<usize>,
    /// The location the value was introduced.
    pub introduction_point: IntroductionPoint,
}

/// The location where a value was introduced.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntroductionPoint {
    /// The statement index of the introduction.
    pub statement_idx: StatementIdx,
    /// The output index of the generating statement of the var.
    pub output_idx: usize,
}

/// A Sierra reference to a value.
/// Returned from a libfunc.
#[derive(Clone, Debug)]
pub struct OutputReferenceValue {
    pub expression: ReferenceExpression,
    pub ty: ConcreteTypeId,
    /// The index of the variable on the continuous-stack.
    pub stack_idx: Option<usize>,
    /// The statememt and output index where the value was introduced.
    /// Statement may be None if it is to be populated later.
    pub introduction_point: OutputReferenceValueIntroductionPoint,
}

/// The location where a value was introduced for output reference values.
#[derive(Clone, Debug)]
pub enum OutputReferenceValueIntroductionPoint {
    /// A new point introduced by a libfunc, the output index is the value.
    New(usize),
    /// Some other known value.
    Existing(IntroductionPoint),
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
    for (param_idx, param) in func.params.iter().rev().enumerate() {
        let size = type_sizes
            .get(&param.ty)
            .ok_or_else(|| ReferencesError::UnknownTypeSize(param.ty.clone()))?;
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
                    stack_idx: None,
                    introduction_point: IntroductionPoint {
                        statement_idx: func.entry_point,
                        output_idx: param_idx,
                    },
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
