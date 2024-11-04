use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind};

use crate::lang::db::{AnalysisDatabase, LsSyntaxGroup};
use crate::lang::lsp::{LsProtoGroup, ToCairo};

mod render;

/// Get hover information at a given text document position.
pub fn hover(params: HoverParams, db: &AnalysisDatabase) -> Option<Hover> {
    let file_id = db.file_for_url(&params.text_document_position_params.text_document.uri)?;
    let position = params.text_document_position_params.position.to_cairo();

    if let Some(hover) = db
        .find_syntax_node_at_position(file_id, position)
        .and_then(|ref node| render::literal(db, node, file_id))
    {
        return Some(hover);
    }

    if let Some(hover) = db
        .find_identifier_at_position(file_id, position)
        .and_then(|ref id| render::definition(db, id, file_id).or_else(|| render::legacy(db, id)))
    {
        return Some(hover);
    }

    None

    // TODO(mkaput): If client only supports plaintext, strip markdown formatting here like RA.
}

/// Convenience shortcut for building hover contents from markdown block.
fn markdown_contents(md: String) -> HoverContents {
    HoverContents::Markup(MarkupContent { kind: MarkupKind::Markdown, value: md })
}
