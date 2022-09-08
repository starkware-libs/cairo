use std::sync::Arc;

use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::db::FilesGroup;
use filesystem::ids::FileId;
use syntax::node::ast::SyntaxFile;
use syntax::node::db::{AsSyntaxGroup, SyntaxGroup};

use crate::parser::{Parser, ParserDiagnostic};

#[cfg(test)]
#[path = "db_test.rs"]
mod db_test;

// Salsa database interface.
#[salsa::query_group(ParserDatabase)]
pub trait ParserGroup: SyntaxGroup + AsSyntaxGroup + FilesGroup {
    fn file_syntax(
        &self,
        file_id: FileId,
    ) -> WithDiagnostics<Option<Arc<SyntaxFile>>, ParserDiagnostic>;
}

#[with_diagnostics]
pub fn file_syntax(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn ParserGroup,
    file_id: FileId,
) -> Option<Arc<SyntaxFile>> {
    let s = db.file_content(file_id)?;
    let parser = Parser::from_text(db.as_syntax_group(), file_id, s.as_str());
    Some(Arc::new(parser.parse_syntax_file().unwrap(diagnostics)))
}
