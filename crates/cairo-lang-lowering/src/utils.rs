use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::{
    BlockId, FlatBlock, FlatBlockEnd, MatchArm, MatchEnumInfo, MatchEnumValue, MatchExternInfo,
    MatchInfo, Statement, StatementCall, StatementConst, StatementDesnap, StatementEnumConstruct,
    StatementSnapshot, StatementStructConstruct, StatementStructDestructure, VarRemapping,
    VarUsage, VariableId,
};

/// A rebuilder trait for rebuilding lowered representation.
pub trait Rebuilder {
    fn map_var_id(&mut self, var: VariableId) -> VariableId;
    fn map_var_usage(&mut self, var_usage: VarUsage) -> VarUsage {
        VarUsage { var_id: self.map_var_id(var_usage.var_id), location: var_usage.location }
    }
    fn map_block_id(&mut self, block: BlockId) -> BlockId;
    fn transform_statement(&mut self, _statement: &mut Statement) {}
    fn transform_remapping(&mut self, _remapping: &mut VarRemapping) {}
    fn transform_end(&mut self, _end: &mut FlatBlockEnd) {}
    fn transform_block(&mut self, _block: &mut FlatBlock) {}
}

pub trait RebuilderEx: Rebuilder {
    /// Rebuilds the statement with renamed var and block ids.
    fn rebuild_statement(&mut self, statement: &Statement) -> Statement {
        let mut statement = match statement {
            Statement::Const(stmt) => Statement::Const(StatementConst {
                value: stmt.value.clone(),
                output: self.map_var_id(stmt.output),
            }),
            Statement::Call(stmt) => Statement::Call(StatementCall {
                function: stmt.function,
                inputs: stmt.inputs.iter().map(|v| self.map_var_usage(*v)).collect(),
                with_coupon: stmt.with_coupon,
                outputs: stmt.outputs.iter().map(|v| self.map_var_id(*v)).collect(),
                location: stmt.location,
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
                variant: stmt.variant.clone(),
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

    /// Apply map_var_id to all the variable in the `remapping`.
    fn rebuild_remapping(&mut self, remapping: &VarRemapping) -> VarRemapping {
        let mut remapping = VarRemapping {
            remapping: OrderedHashMap::from_iter(remapping.iter().map(|(dst, src_var_usage)| {
                (self.map_var_id(*dst), self.map_var_usage(*src_var_usage))
            })),
        };
        self.transform_remapping(&mut remapping);
        remapping
    }

    /// Rebuilds the block end with renamed var and block ids.
    fn rebuild_end(&mut self, end: &FlatBlockEnd) -> FlatBlockEnd {
        let mut end = match end {
            FlatBlockEnd::Return(returns, location) => FlatBlockEnd::Return(
                returns.iter().map(|var_usage| self.map_var_usage(*var_usage)).collect(),
                *location,
            ),
            FlatBlockEnd::Panic(data) => FlatBlockEnd::Panic(self.map_var_usage(*data)),
            FlatBlockEnd::Goto(block_id, remapping) => {
                FlatBlockEnd::Goto(self.map_block_id(*block_id), self.rebuild_remapping(remapping))
            }
            FlatBlockEnd::NotSet => unreachable!(),
            FlatBlockEnd::Match { info } => FlatBlockEnd::Match {
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
                        location: stmt.location,
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
                        location: stmt.location,
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
                        location: stmt.location,
                    }),
                },
            },
        };
        self.transform_end(&mut end);
        end
    }

    /// Rebuilds the block with renamed var and block ids.
    fn rebuild_block(&mut self, block: &FlatBlock) -> FlatBlock {
        let mut statements = vec![];
        for stmt in &block.statements {
            statements.push(self.rebuild_statement(stmt));
        }
        let end = self.rebuild_end(&block.end);
        let mut block = FlatBlock { statements, end };
        self.transform_block(&mut block);
        block
    }
}

impl<T: Rebuilder> RebuilderEx for T {}
