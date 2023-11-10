use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal};

use crate::diagnostic::SemanticDiagnosticKind;
use crate::SemanticDiagnostic;

/// Visibility of an item.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Visibility {
    Public,
    PublicInCrate,
    Private,
}
impl Visibility {
    pub fn from_ast(
        db: &dyn SyntaxGroup,
        diagnostics: &mut DiagnosticsBuilder<SemanticDiagnostic>,
        visibility: &ast::Visibility,
    ) -> Self {
        match visibility {
            ast::Visibility::Pub(visibility_pub) => match visibility_pub.argument_clause(db) {
                ast::OptionVisibilityPubArgumentClause::Empty(_) => Self::Public,
                ast::OptionVisibilityPubArgumentClause::VisibilityPubArgumentClause(argument) => {
                    if argument.argument(db).text(db) == "crate" {
                        Self::PublicInCrate
                    } else {
                        diagnostics.add(SemanticDiagnostic::new(
                            StableLocation::from_ast(&argument),
                            SemanticDiagnosticKind::UnsupportedPubArgument,
                        ));
                        Self::Public
                    }
                }
            },
            ast::Visibility::Default(_) => Self::Private,
        }
    }
}
