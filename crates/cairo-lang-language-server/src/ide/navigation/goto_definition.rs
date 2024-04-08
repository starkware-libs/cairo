use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_utils::Upcast;
use tower_lsp::lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location, Range};

use crate::get_definition_location;
use crate::lang::lsp::{LsProtoGroup, ToLsp};

/// Get the definition location of a symbol at a given text document position.
#[tracing::instrument(
    level = "debug",
    skip_all,
    fields(uri = %params.text_document_position_params.text_document.uri)
)]
pub fn goto_definition(
    params: GotoDefinitionParams,
    db: &RootDatabase,
) -> Option<GotoDefinitionResponse> {
    let file = db.file_for_url(&params.text_document_position_params.text_document.uri);
    let position = params.text_document_position_params.position;
    let (found_file, span) = get_definition_location(db, file, position)?;
    let found_uri = db.url_for_file(found_file);

    let start = span.start.position_in_file(db.upcast(), found_file).unwrap().to_lsp();
    let end = span.end.position_in_file(db.upcast(), found_file).unwrap().to_lsp();
    Some(GotoDefinitionResponse::Scalar(Location { uri: found_uri, range: Range { start, end } }))
}
