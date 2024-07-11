use cairo_lang_syntax::node::ast::Modifier;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::ids::TextId;
use cairo_lang_syntax::node::Terminal;

use crate::diagnostic::SemanticDiagnosticKind::RedundantModifier;
use crate::diagnostic::{SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::Mutability;

/// Returns the mutability of a variable, given the list of modifiers in the AST.
pub fn compute_mutability(
    diagnostics: &mut SemanticDiagnostics,
    syntax_db: &dyn SyntaxGroup,
    modifier_list: &[Modifier],
) -> Mutability {
    let mut last: Option<&Modifier> = None;
    for modifier in modifier_list {
        match last {
            None => {
                last = Some(modifier);
            }
            Some(last) => match modifier {
                Modifier::Ref(terminal) => {
                    diagnostics.report(
                        terminal,
                        RedundantModifier {
                            current_modifier: terminal.text(syntax_db),
                            previous_modifier: get_relevant_modifier(last, syntax_db),
                        },
                    );
                }
                Modifier::Mut(terminal) => {
                    diagnostics.report(
                        terminal,
                        RedundantModifier {
                            current_modifier: terminal.text(syntax_db),
                            previous_modifier: get_relevant_modifier(last, syntax_db),
                        },
                    );
                }
            },
        }
    }
    match last {
        None => Mutability::Immutable,
        Some(Modifier::Mut(_)) => Mutability::Mutable,
        Some(Modifier::Ref(_)) => Mutability::Reference,
    }
}

/// Gets the text of the modifier that causes a variable to have the given mutability status.
fn get_relevant_modifier(modifier: &Modifier, db: &dyn SyntaxGroup) -> TextId {
    match modifier {
        Modifier::Mut(terminal) => terminal.text(db),
        Modifier::Ref(terminal) => terminal.text(db),
    }
}
