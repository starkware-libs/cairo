//! This module implements syntax validation that the parser doesn't handle.
//!
//! A failed validation emits a diagnostic.

use cairo_lang_diagnostics::{DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::SyntaxNode;

use crate::ParserDiagnostic;

/// Validate syntax nodes for aspects that are not handled by the parser.
///
/// This includes things like:
/// 1. Validating string escape sequences.
/// 2. Validating numeric literals that they indeed represent valid values (untyped).
///
/// Not all usage places of the parser require this pass. Primary example is Cairo formatter - it
/// should work even if strings contain invalid escape sequences in strings.
pub fn validate(
    root: SyntaxNode,
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    file_id: FileId,
) -> Maybe<()> {
    todo!()
}
