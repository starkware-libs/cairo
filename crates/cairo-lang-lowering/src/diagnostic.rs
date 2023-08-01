use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::{
    DiagnosticAdded, DiagnosticEntry, DiagnosticLocation, DiagnosticNote, Diagnostics,
    DiagnosticsBuilder,
};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::InferenceError;
use cairo_lang_semantic::TypeId;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;

use crate::Location;

pub struct LoweringDiagnostics {
    pub diagnostics: DiagnosticsBuilder<LoweringDiagnostic>,
    pub file_id: FileId,
}
impl LoweringDiagnostics {
    pub fn new(file_id: FileId) -> Self {
        Self { file_id, diagnostics: DiagnosticsBuilder::default() }
    }
    pub fn build(self) -> Diagnostics<LoweringDiagnostic> {
        self.diagnostics.build()
    }
    pub fn report(
        &mut self,
        stable_ptr: SyntaxStablePtrId,
        kind: LoweringDiagnosticKind,
    ) -> DiagnosticAdded {
        self.report_by_location(Location::new(StableLocation::new(stable_ptr)), kind)
    }
    pub fn report_by_location(
        &mut self,
        location: Location,
        kind: LoweringDiagnosticKind,
    ) -> DiagnosticAdded {
        self.diagnostics.add(LoweringDiagnostic { location, kind })
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LoweringDiagnostic {
    pub location: Location,
    pub kind: LoweringDiagnosticKind,
}
impl DiagnosticEntry for LoweringDiagnostic {
    type DbType = dyn SemanticGroup;

    fn format(&self, db: &Self::DbType) -> String {
        match &self.kind {
            LoweringDiagnosticKind::Unreachable { .. } => "Unreachable code".into(),
            LoweringDiagnosticKind::NonZeroValueInMatch => {
                "Match with a non-zero value is not supported.".into()
            }
            LoweringDiagnosticKind::OnlyMatchZeroIsSupported => {
                "Only match zero (match ... { 0 => ..., _ => ... }) is currently supported.".into()
            }
            LoweringDiagnosticKind::VariableMoved { .. } => "Variable was previously moved.".into(),
            LoweringDiagnosticKind::VariableNotDropped { .. } => "Variable not dropped.".into(),
            LoweringDiagnosticKind::DesnappingANonCopyableType { .. } => {
                "Cannot desnap a non copyable type.".into()
            }
            LoweringDiagnosticKind::UnsupportedMatchedValue => "Unsupported matched value. \
                                                                Currently, only matches on enums \
                                                                and felt252s are supported."
                .into(),
            LoweringDiagnosticKind::UnsupportedMatchArms => "Unsupported match. Currently, \
                                                             matches require one arm per variant, \
                                                             in the order of variant definition."
                .into(),
            LoweringDiagnosticKind::UnsupportedMatchArmNotAVariant => {
                "Unsupported match arm - not a variant.".into()
            }
            LoweringDiagnosticKind::UnsupportedMatchArmOutOfOrder => {
                "Unsupported match arm - variants must be the same order as enum declaration."
                    .into()
            }
            LoweringDiagnosticKind::CannotInlineFunctionThatMightCallItself => {
                "Cannot inline a function that might call itself.".into()
            }
            LoweringDiagnosticKind::MemberPathLoop => {
                "Currently, loops must change the entire variable.".into()
            }
            LoweringDiagnosticKind::UnexpectedError => {
                "Unexpected error has occured, Please submit a full bug report. \
                See https://github.com/starkware-libs/cairo/issues/new/choose for instructions.\
                "
                .into()
            }
            LoweringDiagnosticKind::LiteralOutOfRange { ty } => format!("The value does not fit within the range of type {}.", ty.format(db.upcast())),
            LoweringDiagnosticKind::NoLiteralFunctionFound => {
                "A literal with this type cannot be created.".into()
            }
        }
    }

    fn notes(&self, _db: &Self::DbType) -> &[DiagnosticNote] {
        &self.location.notes
    }

    #[allow(unreachable_patterns, clippy::single_match)]
    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        match &self.kind {
            LoweringDiagnosticKind::Unreachable { last_statement_ptr } => {
                return self
                    .location
                    .stable_location
                    .diagnostic_location_until(db.upcast(), *last_statement_ptr);
            }
            _ => {}
        }
        self.location.stable_location.diagnostic_location(db.upcast())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LoweringDiagnosticKind {
    Unreachable { last_statement_ptr: SyntaxStablePtrId },
    // TODO(lior): Remove once supported.
    NonZeroValueInMatch,
    // TODO(lior): Remove once supported.
    OnlyMatchZeroIsSupported,
    VariableMoved { inference_error: InferenceError },
    VariableNotDropped { drop_err: InferenceError, destruct_err: InferenceError },
    DesnappingANonCopyableType { inference_error: InferenceError },
    UnsupportedMatchedValue,
    UnsupportedMatchArms,
    UnexpectedError,
    UnsupportedMatchArmNotAVariant,
    UnsupportedMatchArmOutOfOrder,
    CannotInlineFunctionThatMightCallItself,
    MemberPathLoop,
    LiteralOutOfRange { ty: TypeId },
    NoLiteralFunctionFound,
}
