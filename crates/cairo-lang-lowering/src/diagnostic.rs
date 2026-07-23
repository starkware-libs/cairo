use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::{
    DiagnosticAdded, DiagnosticEntry, DiagnosticNote, DiagnosticsBuilder, Severity, error_code,
};
use cairo_lang_filesystem::ids::SpanInFile;
use cairo_lang_semantic as semantic;
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

#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::SalsaValue)]
pub struct LoweringDiagnostic<'db> {
    pub location: Location<'db>,
    pub kind: LoweringDiagnosticKind<'db>,
}

impl<'db> DiagnosticEntry<'db> for LoweringDiagnostic<'db> {
    fn format(&self, _db: &'db dyn Database) -> String {
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
            LoweringDiagnosticKind::RefutablePattern => "Refutable pattern in irrefutable \
                                                          binding. Refutable patterns are only \
                                                          supported in `match`, `if let`, \
                                                          `while let`, and `let ... else`."
                .into(),
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

    fn location(&self, db: &'db dyn Database) -> SpanInFile<'db> {
        if let LoweringDiagnosticKind::Unreachable { block_end_ptr } = &self.kind {
            return self.location.stable_location.span_in_file_until(db, *block_end_ptr);
        }
        self.location.stable_location.span_in_file(db)
    }

    fn error_code(&self) -> Option<cairo_lang_diagnostics::ErrorCode> {
        Some(match &self.kind {
            LoweringDiagnosticKind::Unreachable { .. } => error_code!(E3000),
            LoweringDiagnosticKind::VariableMoved { .. } => error_code!(E3001),
            LoweringDiagnosticKind::VariableNotDropped { .. } => error_code!(E3002),
            LoweringDiagnosticKind::DesnappingANonCopyableType { .. } => error_code!(E3003),
            LoweringDiagnosticKind::MatchError(_) => error_code!(E3004),
            LoweringDiagnosticKind::CannotInlineFunctionThatMightCallItself => error_code!(E3005),
            LoweringDiagnosticKind::MemberPathLoop => error_code!(E3006),
            LoweringDiagnosticKind::UnexpectedError => error_code!(E3007),
            LoweringDiagnosticKind::NoPanicFunctionCycle => error_code!(E3008),
            LoweringDiagnosticKind::RefutablePattern => error_code!(E3010),
            LoweringDiagnosticKind::Unsupported => error_code!(E3011),
            LoweringDiagnosticKind::FixedSizeArrayNonCopyableType => error_code!(E3012),
            LoweringDiagnosticKind::EmptyRepeatedElementFixedSizeArray => error_code!(E3013),
        })
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

#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::SalsaValue)]
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
    FixedSizeArrayNonCopyableType,
    EmptyRepeatedElementFixedSizeArray,
    RefutablePattern,
    Unsupported,
}

/// Error in a match-like construct.
/// contains which construct the error occurred in and the error itself.
#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::SalsaValue)]
pub struct MatchError<'db> {
    pub kind: MatchKind<'db>,
    pub error: MatchDiagnostic,
}

/// The type of branch construct the error occurred in.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, salsa::SalsaValue)]
pub enum MatchKind<'db> {
    Match,
    IfLet,
    WhileLet(semantic::ExprId, SyntaxStablePtrId<'db>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::SalsaValue)]
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
