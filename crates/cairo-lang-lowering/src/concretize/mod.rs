use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::substitution::{
    GenericSubstitution, SemanticRewriter, SubstitutionRewriter,
};
use cairo_lang_utils::{Intern, LookupIntern};

use crate::db::LoweringGroup;
use crate::ids::{FunctionId, FunctionLongId, GeneratedFunction};
use crate::{FlatBlockEnd, FlatLowered, MatchArm, Statement};

/// Rewrites a [FunctionId] with a [SubstitutionRewriter].
fn concretize_function(
    db: &dyn LoweringGroup,
    rewriter: &mut SubstitutionRewriter<'_>,
    function: FunctionId,
) -> Maybe<FunctionId> {
    let long_id = match function.lookup_intern(db) {
        FunctionLongId::Semantic(id) => FunctionLongId::Semantic(rewriter.rewrite(id)?),
        FunctionLongId::Generated(GeneratedFunction { parent, key }) => {
            FunctionLongId::Generated(GeneratedFunction { parent: rewriter.rewrite(parent)?, key })
        }
    };
    Ok(long_id.intern(db))
}

/// Concretizes a lowered generic function by applying a generic parameter substitution on its
/// variable types, variants and called functions.
pub fn concretize_lowered(
    db: &dyn LoweringGroup,
    lowered: &mut FlatLowered,
    substitution: &GenericSubstitution,
) -> Maybe<()> {
    let mut rewriter = SubstitutionRewriter { db: db.upcast(), substitution };
    // Substitute all types.
    for (_, var) in lowered.variables.iter_mut() {
        var.ty = rewriter.rewrite(var.ty)?;

        for impl_id in [&mut var.destruct_impl, &mut var.panic_destruct_impl].into_iter().flatten()
        {
            rewriter.internal_rewrite(impl_id)?;
        }
    }
    // Substitute all statements.
    for block in lowered.blocks.iter_mut() {
        for stmt in block.statements.iter_mut() {
            match stmt {
                Statement::Call(stmt) => {
                    stmt.function = concretize_function(db, &mut rewriter, stmt.function)?;
                }
                Statement::EnumConstruct(stmt) => {
                    stmt.variant = rewriter.rewrite(stmt.variant.clone())?;
                }
                Statement::Const(stmt) => {
                    stmt.value = rewriter.rewrite(stmt.value.clone())?;
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
                    s.function = concretize_function(db, &mut rewriter, s.function)?;
                    s.arms.iter_mut()
                }
                crate::MatchInfo::Value(s) => s.arms.iter_mut(),
            } {
                *selector = rewriter.rewrite(selector.clone())?;
            }
        }
    }
    lowered.signature = rewriter.rewrite(lowered.signature.clone())?;

    Ok(())
}
