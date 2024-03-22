use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_syntax::node::SyntaxNode;
use tower_lsp::lsp_types::{
    CodeAction, CodeActionOrCommand, CodeActionParams, CodeActionResponse, Diagnostic,
    NumberOrString,
};
use tracing::debug;

use crate::get_node_and_lookup_items;
use crate::lang::lsp::LsProtoGroup;

mod rename_unused_variable;

/// Compute commands for a given text document and range. These commands are typically code fixes to
/// either fix problems or to beautify/refactor code.
#[tracing::instrument(
    level = "debug",
    skip_all,
    fields(uri = %params.text_document.uri)
)]
pub fn code_actions(params: CodeActionParams, db: &RootDatabase) -> Option<CodeActionResponse> {
    let mut actions = Vec::with_capacity(params.context.diagnostics.len());
    let file_id = db.file_for_url(&params.text_document.uri);
    let (node, _lookup_items) = get_node_and_lookup_items(db, file_id, params.range.start)?;
    for diagnostic in params.context.diagnostics.iter() {
        actions.extend(
            get_code_actions_for_diagnostic(db, &node, diagnostic, &params)
                .into_iter()
                .map(CodeActionOrCommand::from),
        );
    }
    Some(actions)
}

/// Generate code actions for a given diagnostic.
///
/// # Arguments
///
/// * `db` - A reference to the Salsa database.
/// * `node` - The syntax node where the diagnostic is located.
/// * `diagnostic` - The diagnostic for which to generate code actions.
/// * `params` - The parameters for the code action request.
///
/// # Returns
///
/// A vector of [`CodeAction`] objects that can be applied to resolve the diagnostic.
fn get_code_actions_for_diagnostic(
    db: &dyn SemanticGroup,
    node: &SyntaxNode,
    diagnostic: &Diagnostic,
    params: &CodeActionParams,
) -> Vec<CodeAction> {
    let code = match &diagnostic.code {
        Some(NumberOrString::String(code)) => code,
        Some(NumberOrString::Number(code)) => {
            debug!("diagnostic code is not a string: `{code}`");
            return vec![];
        }
        None => {
            debug!("diagnostic code is missing");
            return vec![];
        }
    };

    match code.as_str() {
        "E0001" => {
            vec![rename_unused_variable::rename_unused_variable(
                db,
                node,
                diagnostic.clone(),
                params.text_document.uri.clone(),
            )]
        }
        code => {
            debug!("no code actions for diagnostic code: {code}");
            vec![]
        }
    }
}
