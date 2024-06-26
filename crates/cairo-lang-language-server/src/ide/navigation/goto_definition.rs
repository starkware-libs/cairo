use cairo_lang_utils::Upcast;
use tower_lsp::lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location};

use crate::get_definition_location;
use crate::lang::db::AnalysisDatabase;
use crate::lang::lsp::{LsProtoGroup, ToCairo, ToLsp};

/// Get the definition location of a symbol at a given text document position.
#[tracing::instrument(
    level = "debug",
    skip_all,
    fields(uri = %params.text_document_position_params.text_document.uri)
)]
pub fn goto_definition(
    params: GotoDefinitionParams,
    db: &AnalysisDatabase,
) -> Option<GotoDefinitionResponse> {
    let file = db.file_for_url(&params.text_document_position_params.text_document.uri)?;
    let position = params.text_document_position_params.position.to_cairo();
    let (found_file, span) = get_definition_location(db, file, position)?;
    let found_uri = db.url_for_file(found_file);

    let range = span.position_in_file(db.upcast(), found_file)?.to_lsp();
    Some(GotoDefinitionResponse::Scalar(Location { uri: found_uri, range }))
}
