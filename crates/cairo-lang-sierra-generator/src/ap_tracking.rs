use cairo_lang_lowering::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use cairo_lang_lowering::{BlockId, FlatLowered, MatchInfo, Statement, VarRemapping, VariableId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::Itertools;

#[derive(Default)]
pub struct ApTrackingData {
    // blocks where ap tracking should be enabled.
    pub enable_ap_tracking: UnorderedHashSet<BlockId>,

    // blocks where ap tracking should be disabled.
    pub disable_ap_tracking: UnorderedHashSet<BlockId>,
}

pub fn get_ap_tracking_data(
    lowered_function: &FlatLowered,
    known_ap_change: bool,
    vars_of_interest: OrderedHashSet<VariableId>,
) -> ApTrackingData {
    let mut ctx = ApTrackingContext {
        vars_of_interest,
        data: ApTrackingData {
            enable_ap_tracking: UnorderedHashSet::default(),
            disable_ap_tracking: UnorderedHashSet::default(),
        },
    };

    if ctx.vars_of_interest.is_empty() {
        if !known_ap_change {
            ctx.data.disable_ap_tracking.insert(BlockId::root());
        }

        return ctx.data;
    }

    let mut analysis =
        BackAnalysis { lowered: lowered_function, cache: Default::default(), analyzer: ctx };
    analysis.get_root_info();

    analysis.analyzer.data
}

pub struct ApTrackingContext {
    // The variables that require ap alignment.
    pub vars_of_interest: OrderedHashSet<VariableId>,

    // The data that is collected during the analysis.
    pub data: ApTrackingData,
}

#[derive(Clone)]
pub struct ApTrackingInfo {
    vars: OrderedHashMap<VariableId, Vec<BlockId>>,
}

impl ApTrackingInfo {
    pub fn variables_used(
        &mut self,
        ctx: &ApTrackingContext,
        vars: &[VariableId],
        block_id: BlockId,
    ) {
        for var_id in vars {
            if !ctx.vars_of_interest.contains(var_id) {
                continue;
            }

            match self.vars.entry(*var_id) {
                indexmap::map::Entry::Occupied(mut e) => {
                    let blocks = e.get_mut();
                    if blocks.last() != Some(&block_id) {
                        blocks.push(block_id);
                    }
                }
                indexmap::map::Entry::Vacant(e) => {
                    e.insert(vec![block_id]);
                }
            }
        }
    }
}

impl Analyzer<'_> for ApTrackingContext {
    type Info = ApTrackingInfo;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        (block_id, _statement_index): StatementLocation,
        stmt: &Statement,
    ) {
        for var_id in stmt.outputs() {
            info.vars.swap_remove(&var_id);
        }

        info.variables_used(self, &stmt.inputs(), block_id);
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        (block_id, _statement_index): StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        for dst in remapping.keys() {
            info.vars.swap_remove(dst);
        }

        if info.vars.is_empty() {
            self.data.disable_ap_tracking.insert(block_id);
        }

        info.variables_used(self, remapping.values().cloned().collect_vec().as_slice(), block_id);
    }

    fn merge_match(
        &mut self,
        (block_id, _statement_index): StatementLocation,
        match_info: &MatchInfo,
        infos: &[Self::Info],
    ) -> Self::Info {
        let mut vars = OrderedHashMap::<VariableId, OrderedHashMap<BlockId, usize>>::default();

        for info in infos {
            for (var_id, blocks) in info.vars.iter() {
                // Note that the variables that are introduced in this arm can't be used
                // in other arms so they will be filtered below.

                let var_blocks = vars.entry(*var_id).or_default();
                for block_id in blocks {
                    *(var_blocks.entry(*block_id).or_default()) += 1;
                }
            }
        }

        let mut info = Self::Info {
            vars: OrderedHashMap::from_iter(vars.iter().map(|(var_id, usage)| {
                (
                    *var_id,
                    usage
                        .iter()
                        .filter_map(
                            |(block_id, usage)| if *usage > 1 { Some(*block_id) } else { None },
                        )
                        .sorted_by_key(|block_id| std::cmp::Reverse(block_id.0))
                        .collect(),
                )
            })),
        };

        if !info.vars.is_empty() {
            self.data.enable_ap_tracking.insert(block_id);
        } else {
            self.data.disable_ap_tracking.insert(block_id);
        }

        info.variables_used(self, &match_info.inputs(), block_id);
        info
    }

    fn info_from_return(
        &mut self,
        (block_id, _statement_index): StatementLocation,
        vars: &[VariableId],
    ) -> Self::Info {
        let mut info = Self::Info { vars: Default::default() };
        info.variables_used(self, vars, block_id);
        info
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        _data: &VariableId,
    ) -> Self::Info {
        unreachable!("Panics should have been stripped in a previous phase.");
    }
}
