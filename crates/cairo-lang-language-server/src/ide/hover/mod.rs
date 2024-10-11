use tower_lsp::lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind};

use crate::lang::db::{AnalysisDatabase, LsSyntaxGroup};
use crate::lang::lsp::{LsProtoGroup, ToCairo};

mod range;
mod render;

pub use self::render::Literal;

/// Get hover information at a given text document position.
#[tracing::instrument(
    level = "debug",
    skip_all,
    fields(uri = %params.text_document_position_params.text_document.uri)
)]
pub fn hover(params: HoverParams, db: &AnalysisDatabase) -> Option<Hover> {
    let file_id = db.file_for_url(&params.text_document_position_params.text_document.uri)?;
    let position = params.text_document_position_params.position.to_cairo();

    if let Some(hover) =
        db.find_literal_at_position(file_id, position).map(|literal| literal.render(db, file_id))
    {
        return hover;
    }

    if let Some(hover) = db
        .find_identifier_at_position(file_id, position)
        .map(|ref id| render::definition(db, &id, file_id).or_else(|| render::legacy(db, &id)))
    {
        return hover;
    }

    return None;
}

/// Convenience shortcut for building hover contents from markdown block.
fn markdown_contents(md: String) -> HoverContents {
    HoverContents::Markup(MarkupContent { kind: MarkupKind::Markdown, value: md })
}
