use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::ids::LocationId;
use crate::{
    Block, BlockEnd, BlockId, MatchArm, MatchEnumInfo, MatchEnumValue, MatchExternInfo, MatchInfo,
    Statement, StatementCall, StatementConst, StatementDesnap, StatementEnumConstruct,
    StatementSnapshot, StatementStructConstruct, StatementStructDestructure, VarRemapping,
    VarUsage, VariableId,
};

/// Options for the `inlining-strategy` arguments.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum InliningStrategy {
    /// Do not override inlining strategy.
    ///
    /// Note: equivalent to `InlineSmallFunctions(DEFAULT_INLINE_SMALL_FUNCTIONS_THRESHOLD)`.
    #[default]
    Default,
    /// Should inline small functions up to the given weight.
    ///
    /// Note: the weight exact definition is subject to change.
    InlineSmallFunctions(usize),
    /// Inline only in the case of an `inline(always)` annotation.
    Avoid,
}

/// A rebuilder trait for rebuilding lowered representation.
pub trait Rebuilder<'db> {
    fn map_var_id(&mut self, var: VariableId) -> VariableId;
    fn map_var_usage(&mut self, var_usage: VarUsage<'db>) -> VarUsage<'db> {
        VarUsage {
            var_id: self.map_var_id(var_usage.var_id),
            location: self.map_location(var_usage.location),
        }
    }
    fn map_location(&mut self, location: LocationId<'db>) -> LocationId<'db> {
        location
    }
    fn map_block_id(&mut self, block: BlockId) -> BlockId {
        block
    }
    fn transform_statement(&mut self, _statement: &mut Statement<'db>) {}
    fn transform_remapping(&mut self, _remapping: &mut VarRemapping<'db>) {}
    fn transform_end(&mut self, _end: &mut BlockEnd<'db>) {}
    fn transform_block(&mut self, _block: &mut Block<'db>) {}
}

pub trait RebuilderEx<'db>: Rebuilder<'db> {
    /// Rebuilds the statement with renamed var and block ids.
    fn rebuild_statement(&mut self, statement: &Statement<'db>) -> Statement<'db> {
        let mut statement = match statement {
            Statement::Const(stmt) => Statement::Const(StatementConst::new(
                stmt.value,
                self.map_var_id(stmt.output),
                stmt.boxed,
            )),
            Statement::Call(stmt) => Statement::Call(StatementCall {
                function: stmt.function,
                inputs: stmt.inputs.iter().map(|v| self.map_var_usage(*v)).collect(),
                with_coupon: stmt.with_coupon,
                outputs: stmt.outputs.iter().map(|v| self.map_var_id(*v)).collect(),
                location: self.map_location(stmt.location),
                is_specialization_base_call: stmt.is_specialization_base_call,
            }),
            Statement::StructConstruct(stmt) => {
                Statement::StructConstruct(StatementStructConstruct {
                    inputs: stmt.inputs.iter().map(|v| self.map_var_usage(*v)).collect(),
                    output: self.map_var_id(stmt.output),
                })
            }
            Statement::StructDestructure(stmt) => {
                Statement::StructDestructure(StatementStructDestructure {
                    input: self.map_var_usage(stmt.input),
                    outputs: stmt.outputs.iter().map(|v| self.map_var_id(*v)).collect(),
                })
            }
            Statement::EnumConstruct(stmt) => Statement::EnumConstruct(StatementEnumConstruct {
                variant: stmt.variant,
                input: self.map_var_usage(stmt.input),
                output: self.map_var_id(stmt.output),
            }),
            Statement::Snapshot(stmt) => Statement::Snapshot(StatementSnapshot::new(
                self.map_var_usage(stmt.input),
                self.map_var_id(stmt.original()),
                self.map_var_id(stmt.snapshot()),
            )),
            Statement::Desnap(stmt) => Statement::Desnap(StatementDesnap {
                input: self.map_var_usage(stmt.input),
                output: self.map_var_id(stmt.output),
            }),
        };
        self.transform_statement(&mut statement);
        statement
    }

    /// Apply map_var_id to all the variables in the `remapping`.
    fn rebuild_remapping(&mut self, remapping: &VarRemapping<'db>) -> VarRemapping<'db> {
        let mut remapping = VarRemapping {
            remapping: OrderedHashMap::from_iter(remapping.iter().map(|(dst, src_var_usage)| {
                (self.map_var_id(*dst), self.map_var_usage(*src_var_usage))
            })),
        };
        self.transform_remapping(&mut remapping);
        remapping
    }

    /// Rebuilds the block end with renamed var and block ids.
    fn rebuild_end(&mut self, end: &BlockEnd<'db>) -> BlockEnd<'db> {
        let mut end = match end {
            BlockEnd::Return(returns, location) => BlockEnd::Return(
                returns.iter().map(|var_usage| self.map_var_usage(*var_usage)).collect(),
                self.map_location(*location),
            ),
            BlockEnd::Panic(data) => BlockEnd::Panic(self.map_var_usage(*data)),
            BlockEnd::Goto(block_id, remapping) => {
                BlockEnd::Goto(self.map_block_id(*block_id), self.rebuild_remapping(remapping))
            }
            BlockEnd::NotSet => unreachable!(),
            BlockEnd::Match { info } => BlockEnd::Match {
                info: match info {
                    MatchInfo::Extern(stmt) => MatchInfo::Extern(MatchExternInfo {
                        function: stmt.function,
                        inputs: stmt.inputs.iter().map(|v| self.map_var_usage(*v)).collect(),
                        arms: stmt
                            .arms
                            .iter()
                            .map(|arm| MatchArm {
                                arm_selector: arm.arm_selector.clone(),
                                block_id: self.map_block_id(arm.block_id),
                                var_ids: arm
                                    .var_ids
                                    .iter()
                                    .map(|var_id| self.map_var_id(*var_id))
                                    .collect(),
                            })
                            .collect(),
                        location: self.map_location(stmt.location),
                    }),
                    MatchInfo::Enum(stmt) => MatchInfo::Enum(MatchEnumInfo {
                        concrete_enum_id: stmt.concrete_enum_id,
                        input: self.map_var_usage(stmt.input),
                        arms: stmt
                            .arms
                            .iter()
                            .map(|arm| MatchArm {
                                arm_selector: arm.arm_selector.clone(),
                                block_id: self.map_block_id(arm.block_id),
                                var_ids: arm
                                    .var_ids
                                    .iter()
                                    .map(|var_id| self.map_var_id(*var_id))
                                    .collect(),
                            })
                            .collect(),
                        location: self.map_location(stmt.location),
                    }),
                    MatchInfo::Value(stmt) => MatchInfo::Value(MatchEnumValue {
                        num_of_arms: stmt.num_of_arms,
                        input: self.map_var_usage(stmt.input),
                        arms: stmt
                            .arms
                            .iter()
                            .map(|arm| MatchArm {
                                arm_selector: arm.arm_selector.clone(),
                                block_id: self.map_block_id(arm.block_id),
                                var_ids: arm
                                    .var_ids
                                    .iter()
                                    .map(|var_id| self.map_var_id(*var_id))
                                    .collect(),
                            })
                            .collect(),
                        location: self.map_location(stmt.location),
                    }),
                },
            },
        };
        self.transform_end(&mut end);
        end
    }

    /// Rebuilds the block with renamed var and block ids.
    fn rebuild_block(&mut self, block: &Block<'db>) -> Block<'db> {
        let statements = block.statements.iter().map(|stmt| self.rebuild_statement(stmt)).collect();
        let end = self.rebuild_end(&block.end);
        let mut block = Block { statements, end };
        self.transform_block(&mut block);
        block
    }
}

impl<'db, T: Rebuilder<'db>> RebuilderEx<'db> for T {}
