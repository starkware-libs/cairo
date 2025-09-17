use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_syntax::node::{Terminal, ast};
use salsa::Database;

use crate::SemanticDiagnostic;
use crate::diagnostic::SemanticDiagnosticKind;

/// Visibility of an item.
#[derive(Copy, Clone, Debug, PartialEq, Eq, salsa::Update)]
pub enum Visibility {
    Public,
    PublicInCrate,
    Private,
}
impl Visibility {
    pub fn from_ast<'db>(
        db: &'db dyn Database,
        diagnostics: &mut DiagnosticsBuilder<'db, SemanticDiagnostic<'db>>,
        visibility: &ast::Visibility<'db>,
    ) -> Self {
        match visibility {
            ast::Visibility::Pub(visibility_pub) => match visibility_pub.argument_clause(db) {
                ast::OptionVisibilityPubArgumentClause::Empty(_) => Self::Public,
                ast::OptionVisibilityPubArgumentClause::VisibilityPubArgumentClause(argument) => {
                    if argument.argument(db).text(db).long(db) == "crate" {
                        Self::PublicInCrate
                    } else {
                        diagnostics.add(SemanticDiagnostic::new(
                            StableLocation::from_ast(db, &argument),
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

/// Determine whether a module member is visible to user module given the visibility within it,
/// ignoring or forgetting the visibility of the ancestors of the containing module for a moment.
pub fn peek_visible_in<'db>(
    db: &dyn Database,
    visibility_in_module: Visibility,
    containing_module_id: ModuleId<'db>,
    user_module_id: ModuleId<'db>,
) -> bool {
    if containing_module_id == user_module_id {
        return true;
    }
    match visibility_in_module {
        Visibility::Public => true,
        Visibility::PublicInCrate => {
            user_module_id.owning_crate(db) == containing_module_id.owning_crate(db)
        }
        Visibility::Private => db
            .module_ancestors(user_module_id)
            .contains(&db.module_perceived_module(containing_module_id)),
    }
}
