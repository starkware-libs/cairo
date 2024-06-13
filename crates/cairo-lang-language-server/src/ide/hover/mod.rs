use cairo_lang_compiler::db::RootDatabase;
use tower_lsp::lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind};

use crate::lang::db::LsSyntaxGroup;
use crate::lang::lsp::{LsProtoGroup, ToCairo};
use crate::markdown::Markdown;

mod render;

/// Get hover information at a given text document position.
#[tracing::instrument(
    level = "debug",
    skip_all,
    fields(uri = %params.text_document_position_params.text_document.uri)
)]
pub fn hover(params: HoverParams, db: &RootDatabase) -> Option<Hover> {
    let file_id = db.file_for_url(&params.text_document_position_params.text_document.uri)?;
    let position = params.text_document_position_params.position.to_cairo();
    let identifier = db.find_identifier_at_position(file_id, position)?;

    render::definition(db, &identifier, file_id).or_else(|| render::legacy(db, &identifier))

    // TODO(mkaput): If client only supports plaintext, strip markdown formatting here like RA.
}

/// Convenience shortcut for building hover contents from markdown block.
fn markdown_contents(md: Markdown) -> HoverContents {
    HoverContents::Markup(MarkupContent { kind: MarkupKind::Markdown, value: md.into() })
}
