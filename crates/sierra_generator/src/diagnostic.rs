use defs::diagnostic_utils::StableLocation;
use diagnostics::{DiagnosticEntry, DiagnosticLocation};

use crate::db::SierraGenGroup;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Diagnostic {
    SierraGenerator(SierraGeneratorDiagnostic),
    Semantic(semantic::SemanticDiagnostic),
    Parser(parser::ParserDiagnostic),
}
impl DiagnosticEntry for Diagnostic {
    type DbType = dyn SierraGenGroup;

    fn format(&self, db: &Self::DbType) -> String {
        match self {
            Diagnostic::SierraGenerator(diagnostic) => diagnostic.format(db),
            Diagnostic::Semantic(diagnostic) => diagnostic.format(db.as_semantic_group()),
            Diagnostic::Parser(diagnostic) => {
                diagnostic.format(db.as_semantic_group().as_files_group())
            }
        }
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        match self {
            Diagnostic::SierraGenerator(diagnostic) => diagnostic.location(db),
            Diagnostic::Semantic(diagnostic) => diagnostic.location(db.as_semantic_group()),
            Diagnostic::Parser(diagnostic) => {
                diagnostic.location(db.as_semantic_group().as_files_group())
            }
        }
    }
}

impl From<parser::ParserDiagnostic> for Diagnostic {
    fn from(diagnostic: parser::ParserDiagnostic) -> Self {
        Self::Parser(diagnostic)
    }
}
impl From<semantic::SemanticDiagnostic> for Diagnostic {
    fn from(diagnostic: semantic::SemanticDiagnostic) -> Self {
        Self::Semantic(diagnostic)
    }
}
impl From<SierraGeneratorDiagnostic> for Diagnostic {
    fn from(diagnostic: SierraGeneratorDiagnostic) -> Self {
        Self::SierraGenerator(diagnostic)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SierraGeneratorDiagnostic {
    pub stable_location: StableLocation,
    pub kind: SierraGeneratorDiagnosticKind,
}
impl DiagnosticEntry for SierraGeneratorDiagnostic {
    type DbType = dyn SierraGenGroup;

    fn format(&self, _db: &Self::DbType) -> String {
        match self.kind {
            SierraGeneratorDiagnosticKind::NonZeroValueInMatch => {
                "Match with a non-zero value is not supported."
            }
            SierraGeneratorDiagnosticKind::CallLibFuncWithGenericArgs => {
                "Calling a libfunc with generic arguments is not supported yet."
            }
        }
        .into()
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        self.stable_location.diagnostic_location(db.as_defs_group())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SierraGeneratorDiagnosticKind {
    NonZeroValueInMatch,
    CallLibFuncWithGenericArgs,
}
