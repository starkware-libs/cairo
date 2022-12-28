use defs::diagnostic_utils::StableLocation;
use defs::ids::ModuleFileId;
use diagnostics::{
    DiagnosticAdded, DiagnosticEntry, DiagnosticLocation, Diagnostics, DiagnosticsBuilder,
};
use semantic::db::SemanticGroup;
use syntax::node::ids::SyntaxStablePtrId;

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
        self.diagnostics.add(LoweringDiagnostic {
            stable_location: StableLocation::new(self.module_file_id, stable_ptr),
            kind,
        })
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LoweringDiagnostic {
    pub stable_location: StableLocation,
    pub kind: LoweringDiagnosticKind,
}
impl DiagnosticEntry for LoweringDiagnostic {
    type DbType = dyn SemanticGroup;

    fn format(&self, _db: &Self::DbType) -> String {
        match &self.kind {
            LoweringDiagnosticKind::Unreachable { .. } => "Unreachable code".into(),
            LoweringDiagnosticKind::NonZeroValueInMatch => {
                "Match with a non-zero value is not supported.".into()
            }
            LoweringDiagnosticKind::OnlyMatchZeroIsSupported => {
                "Only match zero (match ... { 0 => ..., _ => ... }) is currently supported.".into()
            }
            LoweringDiagnosticKind::VariableMoved => "Variable was previously moved.".into(),
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
    VariableMoved,
}
