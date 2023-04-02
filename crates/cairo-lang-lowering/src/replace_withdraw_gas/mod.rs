use cairo_lang_semantic::corelib::core_withdraw_gas;

use crate::db::LoweringGroup;
use crate::ids::{FunctionLongId, SemanticFunctionIdEx};
use crate::{FlatBlockEnd, FlatLowered, MatchExternInfo, MatchInfo};

/// Main function for the replace_withdraw_gas lowering phase. Replaces `withdraw_gas` calls with
/// `withdraw_gas_all` calls where necessary.
pub fn replace_withdraw_gas(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    for block in lowered.blocks.iter_mut() {
        let new_end = match &block.end {
            FlatBlockEnd::Match { info: MatchInfo::Extern(info) } => {
                match db.lookup_intern_lowering_function(info.function) {
                    FunctionLongId::AutoWithdrawGas => Some(FlatBlockEnd::Match {
                        info: MatchInfo::Extern(MatchExternInfo {
                            function: core_withdraw_gas(db.upcast()).lowered(db),
                            ..info.clone()
                        }),
                    }),
                    _ => None,
                }
            }
            _ => None,
        };

        if let Some(new_end) = new_end {
            block.end = new_end;
        }
    }
}
