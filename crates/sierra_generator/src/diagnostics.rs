use diagnostics::{DiagnosticEntry, DiagnosticLocation};

use crate::db::SierraGenGroup;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Diagnostic {
    SierraGenerator(SierraGeneratorDiagnostic),
    Semantic(semantic::Diagnostic),
}
impl DiagnosticEntry for Diagnostic {
    type DbType = dyn SierraGenGroup;

    fn format(&self, db: &Self::DbType) -> String {
        match self {
            Diagnostic::SierraGenerator(diagnostic) => diagnostic.format(db),
            Diagnostic::Semantic(diagnostic) => diagnostic.format(db.as_semantic_group()),
        }
    }

    fn location(&self, db: &Self::DbType) -> DiagnosticLocation {
        match self {
            Diagnostic::SierraGenerator(diagnostic) => diagnostic.location(db),
            Diagnostic::Semantic(diagnostic) => diagnostic.location(db.as_semantic_group()),
        }
    }
}

// TODO(lior): is this impl really needed?
impl From<parser::parser::ParserDiagnostic> for Diagnostic {
    fn from(diagnostic: parser::parser::ParserDiagnostic) -> Self {
        Self::Semantic(diagnostic.into())
    }
}
impl From<semantic::Diagnostic> for Diagnostic {
    fn from(diagnostic: semantic::Diagnostic) -> Self {
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
    kind: SierraGeneratorDiagnosticKind,
}
impl DiagnosticEntry for SierraGeneratorDiagnostic {
    type DbType = dyn SierraGenGroup;

    fn format(&self, _db: &Self::DbType) -> String {
        todo!()
    }

    fn location(&self, _db: &Self::DbType) -> DiagnosticLocation {
        todo!()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SierraGeneratorDiagnosticKind {}
