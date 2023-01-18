use cairo_lang_semantic::types::{substitute_generics, GenericSubstitution};
use cairo_lang_semantic::{ConcreteVariant, FunctionId, GenericArgumentId};

use crate::db::LoweringGroup;
use crate::FlatLowered;

/// Concretizes a lowered generic function by applying a generic parameter substitution on its
/// variable types, variants and called functions.
pub fn concretize_lowered(
    db: &dyn LoweringGroup,
    lowered: &mut FlatLowered,
    substitution: &GenericSubstitution,
) {
    // Substitute all types.
    for (_, var) in lowered.variables.iter_mut() {
        var.ty = substitute_generics(db.upcast(), substitution, var.ty);
    }
    // Substitute all statements.
    for block in lowered.blocks.0.iter_mut() {
        for stmt in block.statements.iter_mut() {
            match stmt {
                crate::Statement::Call(stmt) => {
                    substitute_function(db, substitution, &mut stmt.function);
                }
                crate::Statement::MatchExtern(stmt) => {
                    substitute_function(db, substitution, &mut stmt.function);
                    for (variant, _) in stmt.arms.iter_mut() {
                        substitute_variant(db, substitution, variant);
                    }
                }
                crate::Statement::MatchEnum(stmt) => {
                    for (variant, _) in stmt.arms.iter_mut() {
                        substitute_variant(db, substitution, variant);
                    }
                }
                crate::Statement::EnumConstruct(stmt) => {
                    substitute_variant(db, substitution, &mut stmt.variant);
                }
                _ => {}
            }
        }
    }
}

/// Substituted generics in a [FunctionId].
fn substitute_function(
    db: &dyn LoweringGroup,
    substitution: &GenericSubstitution,
    function: &mut FunctionId,
) {
    let mut long_function = db.lookup_intern_function(*function);
    substitute_generics_args(db, substitution, &mut long_function.function.generic_args);
    *function = db.intern_function(long_function);
}

/// Substituted generics in a [ConcreteVariant].
fn substitute_variant(
    db: &dyn LoweringGroup,
    substitution: &GenericSubstitution,
    variant: &mut ConcreteVariant,
) {
    variant.ty = substitute_generics(db.upcast(), substitution, variant.ty);
    let mut long_concrete_enum = db.lookup_intern_concrete_enum(variant.concrete_enum_id);
    substitute_generics_args(db, substitution, &mut long_concrete_enum.generic_args);
    variant.concrete_enum_id = db.intern_concrete_enum(long_concrete_enum);
}

/// Substituted generics in a slice of [GenericArgumentId].
fn substitute_generics_args(
    db: &dyn LoweringGroup,
    substitution: &GenericSubstitution,
    generic_args: &mut [GenericArgumentId],
) {
    for arg in generic_args.iter_mut() {
        match arg {
            GenericArgumentId::Type(ty) => {
                *ty = substitute_generics(db.upcast(), substitution, *ty)
            }
            GenericArgumentId::Literal(_) => {}
        }
    }
}
