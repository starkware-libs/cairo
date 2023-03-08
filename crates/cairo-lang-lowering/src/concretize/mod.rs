use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::substitution::{
    GenericSubstitution, SemanticRewriter, SubstitutionRewriter,
};

use crate::db::LoweringGroup;
use crate::{FlatBlockEnd, FlatLowered, MatchArm, Statement};

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
    }
    // Substitute all statements.
    for block in lowered.blocks.0.iter_mut() {
        for stmt in block.statements.iter_mut() {
            match stmt {
                Statement::Call(stmt) => {
                    stmt.function = rewriter.rewrite(stmt.function)?;
                }
                Statement::EnumConstruct(stmt) => {
                    stmt.variant = rewriter.rewrite(stmt.variant.clone())?;
                }
                Statement::Snapshot(_)
                | Statement::Desnap(_)
                | Statement::Literal(_)
                | Statement::StructConstruct(_)
                | Statement::StructDestructure(_) => {}
            }
        }
        if let FlatBlockEnd::Match { info } = &mut block.end {
            match info {
                crate::MatchInfo::Enum(s) => {
                    for MatchArm { variant_id, block_id: _ } in s.arms.iter_mut() {
                        *variant_id = rewriter.rewrite(variant_id.clone())?;
                    }
                }
                crate::MatchInfo::Extern(s) => {
                    s.function = rewriter.rewrite(s.function)?;
                    for MatchArm { variant_id, block_id: _ } in s.arms.iter_mut() {
                        *variant_id = rewriter.rewrite(variant_id.clone())?;
                    }
                }
            }
        }
    }

    Ok(())
}
