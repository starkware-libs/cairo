use smol_str::SmolStr;
use syntax::node::ast::Modifier;
use syntax::node::db::SyntaxGroup;
use syntax::node::Terminal;

use crate::diagnostic::SemanticDiagnosticKind::RedundantModifier;
use crate::diagnostic::SemanticDiagnostics;
use crate::Mutability;

/// Returns the mutability of a variable, given the list of modifiers in the AST.
pub fn compute_mutability(
    diagnostics: &mut SemanticDiagnostics,
    syntax_db: &dyn SyntaxGroup,
    modifier_list: &[Modifier],
) -> Mutability {
    let mut mutability = Mutability::Immutable;

    for modifier in modifier_list {
        match mutability {
            Mutability::Immutable => {
                mutability = match modifier {
                    Modifier::Ref(_) => Mutability::Reference,
                    Modifier::Mut(_) => Mutability::Mutable,
                };
            }
            Mutability::Mutable | Mutability::Reference => match modifier {
                Modifier::Ref(terminal) => {
                    diagnostics.report(
                        terminal,
                        RedundantModifier {
                            current_modifier: terminal.text(syntax_db),
                            previous_modifier: get_relevant_modifier(&mutability),
                        },
                    );
                }
                Modifier::Mut(terminal) => {
                    diagnostics.report(
                        terminal,
                        RedundantModifier {
                            current_modifier: terminal.text(syntax_db),
                            previous_modifier: get_relevant_modifier(&mutability),
                        },
                    );
                }
            },
        }
    }
    mutability
}

/// Gets the text of the modifier that causes a variable to have the given mutability status.
fn get_relevant_modifier(mutability: &Mutability) -> SmolStr {
    match mutability {
        Mutability::Immutable => "",
        Mutability::Mutable => "mut",
        Mutability::Reference => "ref",
    }
    .to_string()
    .into()
}
