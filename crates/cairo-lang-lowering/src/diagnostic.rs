use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::{
    DiagnosticAdded, DiagnosticEntry, DiagnosticLocation, DiagnosticNote, DiagnosticsBuilder,
    Severity,
};
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib::LiteralError;
use cairo_lang_semantic::expr::inference::InferenceError;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use salsa::Database;

use crate::Location;

pub type LoweringDiagnostics<'db> = DiagnosticsBuilder<'db, LoweringDiagnostic<'db>>;
pub trait LoweringDiagnosticsBuilder<'db> {
    fn report(
        &mut self,
        stable_ptr: impl Into<SyntaxStablePtrId<'db>>,
        kind: LoweringDiagnosticKind<'db>,
    ) -> DiagnosticAdded {
        self.report_by_location(Location::new(StableLocation::new(stable_ptr.into())), kind)
    }
    fn report_by_location(
        &mut self,
        location: Location<'db>,
        kind: LoweringDiagnosticKind<'db>,
    ) -> DiagnosticAdded;
}
impl<'db> LoweringDiagnosticsBuilder<'db> for LoweringDiagnostics<'db> {
    fn report_by_location(
        &mut self,
        location: Location<'db>,
        kind: LoweringDiagnosticKind<'db>,
    ) -> DiagnosticAdded {
        self.add(LoweringDiagnostic { location, kind })
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub struct LoweringDiagnostic<'db> {
    pub location: Location<'db>,
    pub kind: LoweringDiagnosticKind<'db>,
}

impl<'db> DiagnosticEntry<'db> for LoweringDiagnostic<'db> {
    fn format(&self, db: &'db dyn Database) -> String {
        match &self.kind {
            LoweringDiagnosticKind::Unreachable { .. } => "Unreachable code".into(),
            LoweringDiagnosticKind::VariableMoved { .. } => "Variable was previously moved.".into(),
            LoweringDiagnosticKind::VariableNotDropped { .. } => "Variable not dropped.".into(),
            LoweringDiagnosticKind::DesnappingANonCopyableType { .. } => {
                "Cannot desnap a non copyable type.".into()
            }
            LoweringDiagnosticKind::MatchError(match_err) => match_err.format(),
            LoweringDiagnosticKind::CannotInlineFunctionThatMightCallItself => {
                "Cannot inline a function that might call itself.".into()
            }
            LoweringDiagnosticKind::MemberPathLoop => {
                "Currently, loops must change the entire variable.".into()
            }
            LoweringDiagnosticKind::UnexpectedError => {
                "Unexpected error has occurred, Please submit a full bug report. \
                See https://github.com/starkware-libs/cairo/issues/new/choose for instructions.\
                "
                .into()
            }
            LoweringDiagnosticKind::NoPanicFunctionCycle => {
                "Call cycle of `nopanic` functions is not allowed.".into()
            }
            LoweringDiagnosticKind::LiteralError(literal_error) => literal_error.format(db),
            LoweringDiagnosticKind::UnsupportedPattern => {
                "Inner patterns are not allowed in this context.".into()
            }
            LoweringDiagnosticKind::Unsupported => "Unsupported feature.".into(),
            LoweringDiagnosticKind::FixedSizeArrayNonCopyableType => {
                "Fixed size array inner type must implement the `Copy` trait when the array size \
                 is greater than 1."
                    .into()
            }
            LoweringDiagnosticKind::EmptyRepeatedElementFixedSizeArray => {
                "Fixed size array repeated element size must be greater than 0.".into()
            }
        }
    }

    fn severity(&self) -> Severity {
        match self.kind {
            LoweringDiagnosticKind::Unreachable { .. }
            | LoweringDiagnosticKind::MatchError(MatchError {
                kind: _,
                error: MatchDiagnostic::UnreachableMatchArm,
            }) => Severity::Warning,
            _ => Severity::Error,
        }
    }

    fn notes(&self, _db: &dyn Database) -> &[DiagnosticNote<'_>] {
        &self.location.notes
    }

    fn location(&self, db: &'db dyn Database) -> DiagnosticLocation<'db> {
        if let LoweringDiagnosticKind::Unreachable { block_end_ptr } = &self.kind {
            return self.location.stable_location.diagnostic_location_until(db, *block_end_ptr);
        }
        self.location.stable_location.diagnostic_location(db)
    }

    fn is_same_kind(&self, other: &Self) -> bool {
        other.kind == self.kind
    }
}

impl<'db> MatchError<'db> {
    fn format(&self) -> String {
        match (&self.error, &self.kind) {
            (MatchDiagnostic::UnsupportedMatchedType(matched_type), MatchKind::Match) => {
                format!("Unsupported matched type. Type: `{matched_type}`.")
            }
            (MatchDiagnostic::UnsupportedMatchedType(matched_type), MatchKind::IfLet) => {
                format!("Unsupported type in if-let. Type: `{matched_type}`.")
            }
            (MatchDiagnostic::UnsupportedMatchedType(matched_type), MatchKind::WhileLet(_, _)) => {
                format!("Unsupported type in while-let. Type: `{matched_type}`.")
            }
            (MatchDiagnostic::UnsupportedMatchedValueTuple, MatchKind::Match) => {
                "Unsupported matched value. Currently, match on tuples only supports enums as \
                 tuple members."
                    .into()
            }
            (MatchDiagnostic::UnsupportedMatchedValueTuple, MatchKind::IfLet) => {
                "Unsupported value in if-let. Currently, if-let on tuples only supports enums as \
                 tuple members."
                    .into()
            }
            (MatchDiagnostic::UnsupportedMatchedValueTuple, MatchKind::WhileLet(_, _)) => {
                "Unsupported value in while-let. Currently, while-let on tuples only supports \
                 enums as tuple members."
                    .into()
            }
            (MatchDiagnostic::UnsupportedMatchArmNotAVariant, _) => {
                "Unsupported pattern - not a variant.".into()
            }
            (MatchDiagnostic::UnsupportedMatchArmNotATuple, _) => {
                "Unsupported pattern - not a tuple.".into()
            }
            (MatchDiagnostic::UnsupportedMatchArmNotALiteral, MatchKind::Match) => {
                "Unsupported match arm - not a literal.".into()
            }
            (MatchDiagnostic::UnsupportedMatchArmNonSequential, MatchKind::Match) => {
                "Unsupported match - numbers must be sequential starting from 0.".into()
            }
            (
                MatchDiagnostic::UnsupportedMatchArmNotALiteral
                | MatchDiagnostic::UnsupportedMatchArmNonSequential,
                MatchKind::IfLet | MatchKind::WhileLet(_, _),
            ) => unreachable!("Numeric values are not supported in if/while-let conditions."),
            (MatchDiagnostic::NonExhaustiveMatch(variant), MatchKind::Match) => {
                format!("Match is non-exhaustive: `{variant}` not covered.")
            }
            (MatchDiagnostic::NonExhaustiveMatch(_), MatchKind::IfLet) => {
                unreachable!("If-let is not required to be exhaustive.")
            }
            (MatchDiagnostic::NonExhaustiveMatch(_), MatchKind::WhileLet(_, _)) => {
                unreachable!("While-let is not required to be exhaustive.")
            }
            (MatchDiagnostic::UnreachableMatchArm, MatchKind::Match) => {
                "Unreachable pattern arm.".into()
            }
            (MatchDiagnostic::UnreachableMatchArm, MatchKind::IfLet) => {
                "Unreachable clause.".into()
            }
            (MatchDiagnostic::UnreachableMatchArm, MatchKind::WhileLet(_, _)) => {
                unreachable!("While-let does not have two arms.")
            }
            (MatchDiagnostic::UnsupportedNumericInLetCondition, MatchKind::Match) => {
                unreachable!("Numeric values are supported in match conditions.")
            }
            (MatchDiagnostic::UnsupportedNumericInLetCondition, MatchKind::IfLet) => {
                "Numeric values are not supported in if-let conditions.".into()
            }
            (MatchDiagnostic::UnsupportedNumericInLetCondition, MatchKind::WhileLet(_, _)) => {
                "Numeric values are not supported in while-let conditions.".into()
            }
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub enum LoweringDiagnosticKind<'db> {
    Unreachable { block_end_ptr: SyntaxStablePtrId<'db> },
    VariableMoved { inference_error: InferenceError<'db> },
    VariableNotDropped { drop_err: InferenceError<'db>, destruct_err: InferenceError<'db> },
    MatchError(MatchError<'db>),
    DesnappingANonCopyableType { inference_error: InferenceError<'db> },
    UnexpectedError,
    CannotInlineFunctionThatMightCallItself,
    MemberPathLoop,
    NoPanicFunctionCycle,
    LiteralError(LiteralError<'db>),
    FixedSizeArrayNonCopyableType,
    EmptyRepeatedElementFixedSizeArray,
    UnsupportedPattern,
    Unsupported,
}

/// Error in a match-like construct.
/// contains which construct the error occurred in and the error itself.
#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub struct MatchError<'db> {
    pub kind: MatchKind<'db>,
    pub error: MatchDiagnostic,
}

/// The type of branch construct the error occurred in.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum MatchKind<'db> {
    Match,
    IfLet,
    WhileLet(semantic::ExprId, SyntaxStablePtrId<'db>),
}

unsafe impl<'db> salsa::Update for MatchKind<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_value = unsafe { &mut *old_pointer };
        match (old_value, &new_value) {
            (MatchKind::Match, MatchKind::Match) | (MatchKind::IfLet, MatchKind::IfLet) => false,
            (MatchKind::WhileLet(expr, end_ptr), MatchKind::WhileLet(new_expr, new_end_ptr)) => {
                if unsafe { SyntaxStablePtrId::maybe_update(end_ptr, *new_end_ptr) } {
                    *expr = *new_expr;
                    true
                } else if expr != new_expr {
                    *end_ptr = *new_end_ptr;
                    *expr = *new_expr;
                    true
                } else {
                    false
                }
            }
            (old_value, new_value) => {
                *old_value = *new_value;
                true
            }
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub enum MatchDiagnostic {
    /// TODO(TomerStarkware): Get rid of the string and pass the type information directly.
    UnsupportedMatchedType(String),
    UnsupportedMatchedValueTuple,
    UnsupportedMatchArmNotAVariant,
    UnsupportedMatchArmNotATuple,

    UnreachableMatchArm,
    NonExhaustiveMatch(String),

    UnsupportedMatchArmNotALiteral,
    UnsupportedMatchArmNonSequential,
    UnsupportedNumericInLetCondition,
}
