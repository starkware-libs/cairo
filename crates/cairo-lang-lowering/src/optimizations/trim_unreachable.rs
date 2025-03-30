#[cfg(test)]
#[path = "trim_unreachable_test.rs"]
mod test;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::{ConcreteTypeId, TypeLongId};

use crate::db::LoweringGroup;
use crate::{FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchInfo, VarUsage};

/// Trims unreachable code.
/// The unreachable code is detected by the introduction of an enum with 0 variants.
pub fn trim_unreachable(db: &dyn LoweringGroup, lowered: &mut FlatLowered) -> Maybe<()> {
    if lowered.blocks.is_empty() {
        return Ok(());
    }

    let semantic_db = db.upcast();
    let mut fixes = vec![];
    for block in lowered.blocks.iter_mut() {
        let FlatBlockEnd::Match { info } = &block.end else {
            continue;
        };

        for arm in info.arms() {
            for output in &arm.var_ids {
                let variable = &lowered.variables[*output];
                let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_emum_id)) =
                    db.lookup_intern_type(variable.ty)
                else {
                    continue;
                };

                let Ok(variants) = db.enum_variants(concrete_emum_id.enum_id(semantic_db)) else {
                    continue;
                };

                if variants.is_empty() {
                    fixes.push((arm.block_id, *output, concrete_emum_id, variable.location));
                }
            }
        }
    }

    for (block_id, output, concrete_enum_id, location) in fixes {
        let block = &mut lowered.blocks[block_id];

        block.statements.truncate(0);
        block.end = FlatBlockEnd::Match {
            info: MatchInfo::Enum(MatchEnumInfo {
                concrete_enum_id,
                input: VarUsage { var_id: output, location },
                arms: vec![],
                location,
            }),
        }
    }
    Ok(())
}
