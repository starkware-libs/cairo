use cairo_lang_defs::diagnostic_utils::{StableLocation, StableLocationOption};
use cairo_lang_defs::ids::ModuleFileId;
use cairo_lang_diagnostics::{
    DiagnosticAdded, DiagnosticEntry, DiagnosticLocation, Diagnostics, DiagnosticsBuilder,
};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::InferenceError;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;

pub struct LoweringDiagnostics {
    pub diagnostics: DiagnosticsBuilder<LoweringDiagnostic>,
    pub module_file_id: ModuleFileId,
}
impl LoweringDiagnostics {
    pub fn new(module_file_id: ModuleFileId) -> Self {
        Self { module_file_id, diagnostics: DiagnosticsBuilder::default() }
    }
    pub fn build(self) -> Diagnostics<LoweringDiagnostic> {
        self.diagnostics.build()
    }
    pub fn report(
        &mut self,
        stable_ptr: SyntaxStablePtrId,
        kind: LoweringDiagnosticKind,
    ) -> DiagnosticAdded {
        self.report_by_location(StableLocationOption::new(self.module_file_id, stable_ptr), kind)
    }
    pub fn report_by_location(
        &mut self,
        stable_location: StableLocationOption,
        kind: LoweringDiagnosticKind,
    ) -> DiagnosticAdded {
        self.diagnostics.add(LoweringDiagnostic { stable_location: stable_location.unwrap(), kind })
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LoweringDiagnostic {
    pub stable_location: StableLocation,
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
            LoweringDiagnosticKind::VariableMoved { inference_error } => {
                format!("Variable was previously moved. {}", inference_error.format(db))
            }
            LoweringDiagnosticKind::VariableNotDropped { drop_err, destruct_err } => {
                format!(
                    "Variable not dropped. {}. {}.",
                    drop_err.format(db),
                    destruct_err.format(db)
                )
            }
            LoweringDiagnosticKind::DesnappingANonCopyableType { inference_error } => {
                format!("Cannot desnap a non copyable type. {}", inference_error.format(db))
            }
            LoweringDiagnosticKind::UnsupportedMatch => "Unsupported match. Currently, matches \
                                                         require one arm per variant, in the \
                                                         order of variant definition."
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
            LoweringDiagnosticKind::UnsupportedMatchEmptyEnum => {
                "Unsupported match - match on empty enums is not supported.".into()
            }
        }
    }

    #[allow(unreachable_patterns, clippy::single_match)]
    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        match &self.kind {
            LoweringDiagnosticKind::Unreachable { last_statement_ptr } => {
                return self
                    .stable_location
                    .diagnostic_location_until(db.upcast(), *last_statement_ptr);
            }
            _ => {}
        }
        self.stable_location.diagnostic_location(db.upcast())
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
    UnsupportedMatch,
    UnsupportedMatchArmNotAVariant,
    UnsupportedMatchArmOutOfOrder,
    UnsupportedMatchEmptyEnum,
    CannotInlineFunctionThatMightCallItself,
}
