use std::sync::Arc;

use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::db::FilesGroup;
use filesystem::ids::FileId;
use syntax::node::ast::SyntaxFile;
use syntax::node::db::{AsGreenInterner, GreenInterner};

use crate::parser::{Parser, ParserDiagnostic};

#[cfg(test)]
#[path = "db_test.rs"]
mod db_test;

// Salsa database interface.
#[salsa::query_group(ParserDatabase)]
pub trait ParserGroup: GreenInterner + AsGreenInterner + FilesGroup {
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
    let mut parser = Parser::from_text(db.as_green_interner(), file_id, s.as_str());
    let res = Arc::new(parser.parse_syntax_file());
    parser.report_diagnostics(diagnostics, file_id);
    Some(res)
}
