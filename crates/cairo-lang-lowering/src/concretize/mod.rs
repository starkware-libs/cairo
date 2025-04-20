use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::substitution::GenericSubstitution;
use cairo_lang_utils::{Intern, LookupIntern};

use crate::db::LoweringGroup;
use crate::ids::{FunctionId, FunctionLongId, GeneratedFunction, SemanticFunctionIdEx};
use crate::{FlatBlockEnd, FlatLowered, MatchArm, Statement};

/// Rewrites a [FunctionId] with a [GenericSubstitution].
fn concretize_function(
    db: &dyn LoweringGroup,
    substitution: &GenericSubstitution,
    function: FunctionId,
) -> Maybe<FunctionId> {
    match function.lookup_intern(db) {
        FunctionLongId::Semantic(id) => {
            // We call `lowered` here in case the function will be substituted to a generated one.
            Ok(substitution.substitute(db.upcast(), id)?.lowered(db))
        }
        FunctionLongId::Generated(GeneratedFunction { parent, key }) => {
            Ok(FunctionLongId::Generated(GeneratedFunction {
                parent: substitution.substitute(db.upcast(), parent)?,
                key,
            })
            .intern(db))
        }
    }
}

/// Concretizes a lowered generic function by applying a generic parameter substitution on its
/// variable types, variants and called functions.
pub fn concretize_lowered(
    db: &dyn LoweringGroup,
    lowered: &mut FlatLowered,
    substitution: &GenericSubstitution,
) -> Maybe<()> {
    // Substitute all types.
    for (_, var) in lowered.variables.iter_mut() {
        var.ty = substitution.substitute(db.upcast(), var.ty)?;

        for impl_id in [&mut var.destruct_impl, &mut var.panic_destruct_impl].into_iter().flatten()
        {
            *impl_id = substitution.substitute(db.upcast(), *impl_id)?;
        }
    }
    // Substitute all statements.
    for block in lowered.blocks.iter_mut() {
        for stmt in block.statements.iter_mut() {
            match stmt {
                Statement::Call(stmt) => {
                    stmt.function = concretize_function(db, substitution, stmt.function)?;
                }
                Statement::EnumConstruct(stmt) => {
                    stmt.variant = substitution.substitute(db.upcast(), stmt.variant.clone())?;
                }
                Statement::Const(stmt) => {
                    stmt.value = substitution.substitute(db.upcast(), stmt.value.clone())?;
                }
                Statement::Snapshot(_)
                | Statement::Desnap(_)
                | Statement::StructConstruct(_)
                | Statement::StructDestructure(_) => {}
            }
        }
        if let FlatBlockEnd::Match { info } = &mut block.end {
            for MatchArm { arm_selector: selector, .. } in match info {
                crate::MatchInfo::Enum(s) => s.arms.iter_mut(),
                crate::MatchInfo::Extern(s) => {
                    s.function = concretize_function(db, substitution, s.function)?;
                    s.arms.iter_mut()
                }
                crate::MatchInfo::Value(s) => s.arms.iter_mut(),
            } {
                *selector = substitution.substitute(db.upcast(), selector.clone())?;
            }
        }
    }
    lowered.signature = substitution.substitute(db.upcast(), lowered.signature.clone())?;

    Ok(())
}
