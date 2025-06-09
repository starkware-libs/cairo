use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::substitution::GenericSubstitution;
use cairo_lang_utils::{Intern, LookupIntern};

use crate::db::LoweringGroup;
use crate::ids::{FunctionId, FunctionLongId, GeneratedFunction, SemanticFunctionIdEx};
use crate::{BlockEnd, Lowered, MatchArm, Statement};

/// Rewrites a [FunctionId] with a [GenericSubstitution].
fn concretize_function(
    db: &dyn LoweringGroup,
    substitution: &GenericSubstitution,
    function: FunctionId,
) -> Maybe<FunctionId> {
    match function.lookup_intern(db) {
        FunctionLongId::Semantic(id) => {
            // We call `lowered` here in case the function will be substituted to a generated one.
            Ok(substitution.substitute(db, id)?.lowered(db))
        }
        FunctionLongId::Generated(GeneratedFunction { parent, key }) => {
            Ok(FunctionLongId::Generated(GeneratedFunction {
                parent: substitution.substitute(db, parent)?,
                key,
            })
            .intern(db))
        }
        FunctionLongId::Specialized(_) => {
            unreachable!("Specialization of functions only occurs post concretization.")
        }
    }
}

/// Concretizes a lowered generic function by applying a generic parameter substitution on its
/// variable types, variants and called functions.
pub fn concretize_lowered(
    db: &dyn LoweringGroup,
    lowered: &mut Lowered,
    substitution: &GenericSubstitution,
) -> Maybe<()> {
    // Substitute all types.
    for (_, var) in lowered.variables.iter_mut() {
        var.ty = substitution.substitute(db, var.ty)?;

        for impl_id in [&mut var.destruct_impl, &mut var.panic_destruct_impl].into_iter().flatten()
        {
            *impl_id = substitution.substitute(db, *impl_id)?;
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
                    stmt.variant = substitution.substitute(db, stmt.variant)?;
                }
                Statement::Const(stmt) => {
                    stmt.value = substitution.substitute(db, stmt.value.clone())?;
                }
                Statement::Snapshot(_)
                | Statement::Desnap(_)
                | Statement::StructConstruct(_)
                | Statement::StructDestructure(_) => {}
            }
        }
        if let BlockEnd::Match { info } = &mut block.end {
            for MatchArm { arm_selector: selector, .. } in match info {
                crate::MatchInfo::Enum(s) => s.arms.iter_mut(),
                crate::MatchInfo::Extern(s) => {
                    s.function = concretize_function(db, substitution, s.function)?;
                    s.arms.iter_mut()
                }
                crate::MatchInfo::Value(s) => s.arms.iter_mut(),
            } {
                *selector = substitution.substitute(db, selector.clone())?;
            }
        }
    }
    lowered.signature = substitution.substitute(db, lowered.signature.clone())?;

    Ok(())
}
