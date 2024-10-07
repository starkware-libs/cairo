use cairo_lang_filesystem::db::get_originating_location;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::{TextPosition, TextSpan};
use cairo_lang_utils::Upcast;
use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location};

use crate::lang::db::{AnalysisDatabase, LsSemanticGroup, LsSyntaxGroup};
use crate::lang::inspect::defs::find_definition;
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

/// Returns the file id and span of the definition of an expression from its position.
///
/// # Arguments
///
/// * `db` - Preloaded compilation database
/// * `uri` - Uri of the expression position
/// * `position` - Position of the expression
///
/// # Returns
///
/// The [FileId] and [TextSpan] of the expression definition if found.
fn get_definition_location(
    db: &AnalysisDatabase,
    file: FileId,
    position: TextPosition,
) -> Option<(FileId, TextSpan)> {
    let identifier = db.find_identifier_at_position(file, position)?;

    let syntax_db = db.upcast();
    let node = db.find_syntax_node_at_position(file, position)?;
    let lookup_items = db.collect_lookup_items_stack(&node)?;
    let (_, stable_ptr) = find_definition(db, &identifier, &lookup_items)?;
    let node = stable_ptr.lookup(syntax_db);
    let found_file = stable_ptr.file_id(syntax_db);
    let span = node.span_without_trivia(syntax_db);
    let width = span.width();
    let (file_id, mut span) = get_originating_location(db.upcast(), found_file, span.start_only());
    span.end = span.end.add_width(width);
    Some((file_id, span))
}
