use std::path::PathBuf;
use std::sync::Arc;

use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder};
use cairo_lang_filesystem::db::{init_files_group, FilesDatabase, FilesGroup};
use cairo_lang_filesystem::ids::{FileId, FileKind, FileLongId, VirtualFile};
use cairo_lang_syntax::node::ast::SyntaxFile;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::{Intern, Upcast};
use smol_str::ToSmolStr;
use thiserror::Error;

use crate::db::ParserDatabase;
use crate::parser::Parser;
use crate::ParserDiagnostic;

/// A salsa database for parsing only.
#[salsa::database(ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct SimpleParserDatabase {
    storage: salsa::Storage<SimpleParserDatabase>,
}
impl salsa::Database for SimpleParserDatabase {}
impl Default for SimpleParserDatabase {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}

impl Upcast<dyn SyntaxGroup> for SimpleParserDatabase {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl Upcast<dyn FilesGroup> for SimpleParserDatabase {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}

impl SimpleParserDatabase {
    pub fn parse_virtual(
        &self,
        content: impl ToString,
    ) -> Result<SyntaxNode, ParsingDiagnosticsAddedError> {
        let (node, diagnostics) = self.parse_virtual_with_diagnostics(content);
        if diagnostics.check_error_free().is_ok() {
            Ok(node)
        } else {
            Err(ParsingDiagnosticsAddedError { diagnostics })
        }
    }

    pub fn parse_virtual_with_diagnostics(
        &self,
        content: impl ToString,
    ) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
        let file = FileLongId::Virtual(VirtualFile {
            parent: None,
            name: "parser_input".to_smolstr(),
            content: Arc::new(content.to_string()),
            code_mappings: Default::default(),
            kind: FileKind::Module,
        })
        .intern(self);
        get_syntax_root_and_diagnostics(self, file, content.to_string().as_str())
    }
}

#[derive(Debug, Error)]
#[error("Diagnostics added")]
pub struct ParsingDiagnosticsAddedError {
    pub diagnostics: Diagnostics<ParserDiagnostic>,
}

/// Reads a cairo file to the db and return the syntax_root and diagnostic of its parsing.
pub fn get_syntax_root_and_diagnostics_from_file(
    db: &SimpleParserDatabase,
    cairo_filepath: PathBuf,
) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
    let file_id = FileId::new(db, cairo_filepath);
    let contents = db.file_content(file_id).unwrap();
    get_syntax_root_and_diagnostics(db, file_id, contents.as_str())
}

/// Returns the syntax_root and diagnostic of a file in the db.
pub fn get_syntax_root_and_diagnostics(
    db: &SimpleParserDatabase,
    file_id: FileId,
    contents: &str,
) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
    let (syntax_file, diagnostics) = get_syntax_file_and_diagnostics(db, file_id, contents);
    (syntax_file.as_syntax_node(), diagnostics)
}

/// Returns the syntax_file and diagnostic of a file in the db.
pub fn get_syntax_file_and_diagnostics(
    db: &SimpleParserDatabase,
    file_id: FileId,
    contents: &str,
) -> (SyntaxFile, Diagnostics<ParserDiagnostic>) {
    let mut diagnostics = DiagnosticsBuilder::default();
    let syntax_file = Parser::parse_file(db, &mut diagnostics, file_id, contents);
    (syntax_file, diagnostics.build())
}
