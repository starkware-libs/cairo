use cairo_lang_casm::ap_change::ApplyApChange;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_sierra::ids::{ConcreteTypeId, VarId};
use cairo_lang_sierra::program::{Function, StatementIdx};
use cairo_lang_sierra_type_size::TypeSizeMap;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::write_comma_separated;
use thiserror::Error;

use crate::invocations::InvocationError;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum ReferencesError {
    #[error("Invalid function declaration.")]
    InvalidFunctionDeclaration(Function),
    #[error(
        "One of the arguments does not match the expected type of the libfunc or return statement."
    )]
    InvalidReferenceTypeForArgument,
    #[error("Unknown type `{0}`.")]
    UnknownType(ConcreteTypeId),
}

pub type StatementRefs = OrderedHashMap<VarId, ReferenceValue>;

/// A Sierra reference to a value.
/// Corresponds to an argument or return value of a Sierra statement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReferenceValue {
    pub expression: ReferenceExpression,
    pub ty: ConcreteTypeId,
    /// The index of the variable on the continuous-stack. 0 represents the oldest element on the
    /// stack.
    pub stack_idx: Option<usize>,
    /// The location the value was introduced.
    pub introduction_point: IntroductionPoint,
}
impl ReferenceValue {
    /// Should never actually fail - since this was built by the type system.
    /// This is just a sanity check, and therefore it panics instead of returning an error.
    pub fn validate(&self, type_sizes: &TypeSizeMap) {
        let size = *type_sizes.get(&self.ty).expect("ReferenceValue has unknown type");
        let actual_size: i16 = self.expression.cells.len().into_or_panic();
        assert_eq!(actual_size, size, "ReferenceValue type size mismatch.");
    }
}

/// The location where a value was introduced.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IntroductionPoint {
    /// The index of the statement creating the value, None if introduced as a function param.
    pub source_statement_idx: Option<StatementIdx>,
    /// The index of the statement the value was created into.
    pub destination_statement_idx: StatementIdx,
    /// The output index of the generating statement of the var.
    pub output_idx: usize,
}

impl core::fmt::Display for IntroductionPoint {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if let Some(source_statement_idx) = self.source_statement_idx {
            write!(
                f,
                "#{source_statement_idx}->#{}[{}]",
                self.destination_statement_idx, self.output_idx
            )
        } else {
            write!(f, "Function@{}[{}]", self.destination_statement_idx, self.output_idx)
        }
    }
}

/// A Sierra reference to a value.
/// Returned from a libfunc.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OutputReferenceValue {
    pub expression: ReferenceExpression,
    pub ty: ConcreteTypeId,
    /// The index of the variable on the continuous-stack.
    pub stack_idx: Option<usize>,
    /// The statement and output index where the value was introduced.
    /// Statement may be New if it is to be populated later.
    pub introduction_point: OutputReferenceValueIntroductionPoint,
}

/// The location where a value was introduced for output reference values.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OutputReferenceValueIntroductionPoint {
    /// A new point introduced by a libfunc. The inner value is the output index.
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

    /// Builds a zero-sized reference expression.
    pub fn zero_sized() -> Self {
        Self { cells: vec![] }
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

impl core::fmt::Display for ReferenceExpression {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "[")?;
        write_comma_separated(f, &self.cells)?;
        write!(f, "]")
    }
}

/// Builds the HashMap of references to the parameters of a function.
pub fn build_function_parameters_refs(
    func: &Function,
    type_sizes: &TypeSizeMap,
) -> Result<StatementRefs, ReferencesError> {
    let mut refs = StatementRefs::default();
    let mut offset = -3_i16;
    for (param_idx, param) in func.params.iter().rev().enumerate() {
        let size = type_sizes
            .get(&param.ty)
            .ok_or_else(|| ReferencesError::UnknownType(param.ty.clone()))?;
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
                        source_statement_idx: None,
                        destination_statement_idx: func.entry_point,
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
