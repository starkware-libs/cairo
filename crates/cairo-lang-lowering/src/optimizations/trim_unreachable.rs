#[cfg(test)]
#[path = "trim_unreachable_test.rs"]
mod test;

use cairo_lang_semantic::{ConcreteTypeId, TypeLongId};

use crate::db::LoweringGroup;
use crate::{BlockId, FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchInfo, VarUsage, VariableId};

/// Trims unreachable code.
/// The unreachable code is detected by the introduction of an enum with 0 variants.
pub fn trim_unreachable(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    if lowered.blocks.is_empty() {
        return;
    }

    let semantic_db = db.upcast();
    let mut fixes = vec![];

    // Helper function to process a variable.
    // Checks if the variable is an enum with zero variants.
    // If it is, a fix is added to the list of fixes, and the function returns `true`,
    // Otherwise, it returns `false.
    let mut handle_var = |var_id: &VariableId, introduction_block| {
        let variable = &lowered.variables[*var_id];
        let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_emum_id)) =
            db.lookup_intern_type(variable.ty)
        else {
            return false;
        };

        let Ok(variants) = db.enum_variants(concrete_emum_id.enum_id(semantic_db)) else {
            return false;
        };

        if !variants.is_empty() {
            return false;
        }

        fixes.push((introduction_block, *var_id, concrete_emum_id, variable.location));
        true
    };

    lowered.parameters.iter().find(|param| handle_var(param, BlockId::root()));

    for block in lowered.blocks.iter_mut() {
        let FlatBlockEnd::Match { info } = &block.end else {
            continue;
        };

        for arm in info.arms() {
            arm.var_ids.iter().find(|var_id| handle_var(var_id, arm.block_id));
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
}
