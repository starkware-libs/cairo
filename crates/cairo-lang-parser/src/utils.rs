use std::path::PathBuf;

use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder};
use cairo_lang_filesystem::db::{init_files_group, ExternalFiles, FilesDatabase, FilesGroup};
use cairo_lang_filesystem::ids::{FileId, FileKind, FileLongId, VirtualFile};
use cairo_lang_syntax::node::ast::SyntaxFile;
use cairo_lang_syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::{Intern, Upcast};

use crate::db::ParserDatabase;
use crate::parser::Parser;
use crate::ParserDiagnostic;

/// A salsa database for parsing only.
#[salsa::database(ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct SimpleParserDatabase {
    storage: salsa::Storage<SimpleParserDatabase>,
}
impl salsa::Database for SimpleParserDatabase {}
impl ExternalFiles for SimpleParserDatabase {}
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
    /// Parses new file and returns its syntax root.
    ///
    /// This is similar to [Self::parse_virtual_with_diagnostics], but is more ergonomic in cases
    /// when exact diagnostics do not matter at the usage place. If the parser has emitted error
    /// diagnostics, this function will return an error. If no error diagnostics has been
    /// emitted, the syntax root will be returned.
    pub fn parse_virtual(
        &self,
        content: impl ToString,
    ) -> Result<SyntaxNode, Diagnostics<ParserDiagnostic>> {
        let (node, diagnostics) = self.parse_virtual_with_diagnostics(content);
        if diagnostics.check_error_free().is_ok() { Ok(node) } else { Err(diagnostics) }
    }

    /// Parses new file and return its syntax root with diagnostics.
    ///
    /// This function creates new virtual file with the given content and parses it.
    /// Diagnostics gathered by the parser are returned alongside the result.
    pub fn parse_virtual_with_diagnostics(
        &self,
        content: impl ToString,
    ) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
        let file = FileLongId::Virtual(VirtualFile {
            parent: None,
            name: "parser_input".into(),
            content: content.to_string().into(),
            code_mappings: [].into(),
            kind: FileKind::Module,
        })
        .intern(self);
        get_syntax_root_and_diagnostics(self, file, content.to_string().as_str())
    }
}

/// Reads a cairo file to the db and return the syntax_root and diagnostic of its parsing.
pub fn get_syntax_root_and_diagnostics_from_file(
    db: &SimpleParserDatabase,
    cairo_filepath: PathBuf,
) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
    let file_id = FileId::new(db, cairo_filepath);
    let contents = db.file_content(file_id).unwrap();
    get_syntax_root_and_diagnostics(db, file_id, &contents)
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
