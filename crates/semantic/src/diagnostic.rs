#[cfg(test)]
#[path = "diagnostic_test.rs"]
mod test;

use defs::diagnostic_utils::StableLocation;
use diagnostics::{DiagnosticEntry, DiagnosticLocation};
use parser::ParserDiagnostic;

use crate::db::SemanticGroup;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Diagnostic {
    Semantic(SemanticDiagnostic),
    Parser(ParserDiagnostic),
}
impl DiagnosticEntry for Diagnostic {
    type DbType = dyn SemanticGroup;

    fn format(&self, db: &Self::DbType) -> String {
        match self {
            Diagnostic::Semantic(diagnostic) => diagnostic.format(db),
            Diagnostic::Parser(diagnostic) => diagnostic.format(db.as_files_group()),
        }
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        match self {
            Diagnostic::Semantic(diagnostic) => diagnostic.location(db),
            Diagnostic::Parser(diagnostic) => diagnostic.location(db.as_files_group()),
        }
    }
}
impl From<ParserDiagnostic> for Diagnostic {
    fn from(diagnostic: ParserDiagnostic) -> Self {
        Self::Parser(diagnostic)
    }
}
impl From<SemanticDiagnostic> for Diagnostic {
    fn from(diagnostic: SemanticDiagnostic) -> Self {
        Self::Semantic(diagnostic)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SemanticDiagnostic {
    pub stable_location: StableLocation,
    pub kind: SemanticDiagnosticKind,
}
impl DiagnosticEntry for SemanticDiagnostic {
    type DbType = dyn SemanticGroup;

    fn format(&self, _db: &Self::DbType) -> String {
        match self.kind {
            SemanticDiagnosticKind::UnknownBinaryOperator => "Unknown binary operator.",
            SemanticDiagnosticKind::UnknownFunction => "Unknown function.",
            SemanticDiagnosticKind::UnknownType => "Unknown type.",
        }
        .into()
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        self.stable_location.diagnostic_location(db.as_defs_group())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SemanticDiagnosticKind {
    UnknownBinaryOperator,
    UnknownFunction,
    UnknownType,
}
