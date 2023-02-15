use cairo_lang_semantic::types::{
    substitute_function, substitute_ty, substitute_variant, GenericSubstitution,
};

use crate::db::LoweringGroup;
use crate::{FlatLowered, Statement};

/// Concretizes a lowered generic function by applying a generic parameter substitution on its
/// variable types, variants and called functions.
pub fn concretize_lowered(
    db: &dyn LoweringGroup,
    lowered: &mut FlatLowered,
    substitution: &GenericSubstitution,
) {
    let semantic_db = db.upcast();
    // Substitute all types.
    for (_, var) in lowered.variables.iter_mut() {
        var.ty = substitute_ty(semantic_db, substitution, var.ty);
    }
    // Substitute all statements.
    for block in lowered.blocks.0.iter_mut() {
        for stmt in block.statements.iter_mut() {
            match stmt {
                Statement::Call(stmt) => {
                    substitute_function(semantic_db, substitution, &mut stmt.function);
                }
                Statement::MatchExtern(stmt) => {
                    substitute_function(semantic_db, substitution, &mut stmt.function);
                    for (variant, _) in stmt.arms.iter_mut() {
                        substitute_variant(semantic_db, substitution, variant);
                    }
                }
                Statement::MatchEnum(stmt) => {
                    for (variant, _) in stmt.arms.iter_mut() {
                        substitute_variant(semantic_db, substitution, variant);
                    }
                }
                Statement::EnumConstruct(stmt) => {
                    substitute_variant(semantic_db, substitution, &mut stmt.variant);
                }
                Statement::Snapshot(_)
                | Statement::Literal(_)
                | Statement::StructConstruct(_)
                | Statement::StructDestructure(_) => {}
            }
        }
    }
}
