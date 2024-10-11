use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::ast::{
    TerminalIdentifier, TerminalLiteralNumber, TerminalShortString, TerminalString,
};
use tower_lsp::lsp_types::Hover;

use crate::ide::hover::render;
use crate::lang::db::AnalysisDatabase;

#[derive(Debug, Clone)]
pub enum HoverTarget {
    Identifier(TerminalIdentifier),
    Number(TerminalLiteralNumber),
    String(TerminalString),
    ShortString(TerminalShortString),
}

impl HoverTarget {
    pub fn render(&self, db: &AnalysisDatabase, file_id: FileId) -> Option<Hover> {
        match self {
            Self::Identifier(ref identifier) => render::definition(db, identifier, file_id)
                .or_else(|| render::legacy(db, identifier)),
            Self::Number(ref literal) => render::number(db, literal, file_id),
            Self::String(ref literal) => render::string(db, literal, file_id),
            Self::ShortString(ref literal) => render::short_string(db, literal, file_id),
        }
    }
}
