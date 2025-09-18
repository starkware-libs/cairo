use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_syntax::node::ast::Modifier;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};
use salsa::Database;

use crate::Mutability;
use crate::diagnostic::SemanticDiagnosticKind::RedundantModifier;
use crate::diagnostic::{SemanticDiagnostics, SemanticDiagnosticsBuilder};

/// Returns the mutability of a variable, given the list of modifiers in the AST.
pub fn compute_mutability<'db>(
    diagnostics: &mut SemanticDiagnostics<'db>,
    db: &'db dyn Database,
    modifier_list: &[Modifier<'db>],
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
                        terminal.stable_ptr(db),
                        RedundantModifier {
                            current_modifier: terminal.text(db),
                            // TODO(eytan-starkware): Change &str to SmolStrId.
                            previous_modifier: SmolStrId::from(
                                db,
                                get_relevant_modifier(&mutability),
                            ),
                        },
                    );
                }
                Modifier::Mut(terminal) => {
                    diagnostics.report(
                        terminal.stable_ptr(db),
                        RedundantModifier {
                            current_modifier: terminal.text(db),
                            // TODO(eytan-starkware): Change &str to SmolStrId.
                            previous_modifier: SmolStrId::from(
                                db,
                                get_relevant_modifier(&mutability),
                            ),
                        },
                    );
                }
            },
        }
    }
    mutability
}

/// Gets the text of the modifier that causes a variable to have the given mutability status.
pub fn get_relevant_modifier(mutability: &Mutability) -> &'static str {
    match mutability {
        Mutability::Immutable => "",
        Mutability::Mutable => "mut",
        Mutability::Reference => "ref",
    }
}
