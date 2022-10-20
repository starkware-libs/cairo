use syntax::node::ast::Modifier;
use syntax::node::db::SyntaxGroup;
use syntax::node::Terminal;

use crate::diagnostic::SemanticDiagnosticKind::RepeatedModifier;
use crate::diagnostic::SemanticDiagnostics;
use crate::semantic;

/// Returns the modifiers of a variable, given the list of modifiers in the AST.
pub fn compute_modifiers(
    diagnostics: &mut SemanticDiagnostics,
    syntax_db: &dyn SyntaxGroup,
    modifier_list: &[Modifier],
) -> semantic::Modifiers {
    let mut is_mut = false;
    let mut is_ref = false;

    for modifier in modifier_list {
        match modifier {
            Modifier::Mut(terminal) => {
                if is_mut {
                    diagnostics
                        .report(terminal, RepeatedModifier { modifier: terminal.text(syntax_db) });
                }
                is_mut = true
            }
            Modifier::Ref(terminal) => {
                if is_ref {
                    diagnostics
                        .report(terminal, RepeatedModifier { modifier: terminal.text(syntax_db) });
                }
                is_ref = true
            }
        }
    }

    semantic::Modifiers { is_mut, is_ref }
}
