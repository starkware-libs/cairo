use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::{
    BlockId, FlatBlock, FlatBlockEnd, MatchEnumInfo, MatchExternInfo, MatchInfo, Statement,
    StatementCall, StatementDesnap, StatementEnumConstruct, StatementLiteral, StatementSnapshot,
    StatementStructConstruct, StatementStructDestructure, VarRemapping, VariableId,
};

/// A rebuilder trait for rebuilding lowered representation.
pub trait Rebuilder {
    fn map_var_id(&mut self, var: VariableId) -> VariableId;
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
            Statement::Literal(stmt) => Statement::Literal(StatementLiteral {
                value: stmt.value.clone(),
                output: self.map_var_id(stmt.output),
            }),
            Statement::Call(stmt) => Statement::Call(StatementCall {
                function: stmt.function,
                inputs: stmt.inputs.iter().map(|v| self.map_var_id(*v)).collect(),
                outputs: stmt.outputs.iter().map(|v| self.map_var_id(*v)).collect(),
                location: stmt.location,
            }),
            Statement::StructConstruct(stmt) => {
                Statement::StructConstruct(StatementStructConstruct {
                    inputs: stmt.inputs.iter().map(|v| self.map_var_id(*v)).collect(),
                    output: self.map_var_id(stmt.output),
                })
            }
            Statement::StructDestructure(stmt) => {
                Statement::StructDestructure(StatementStructDestructure {
                    input: self.map_var_id(stmt.input),
                    outputs: stmt.outputs.iter().map(|v| self.map_var_id(*v)).collect(),
                })
            }
            Statement::EnumConstruct(stmt) => Statement::EnumConstruct(StatementEnumConstruct {
                variant: stmt.variant.clone(),
                input: self.map_var_id(stmt.input),
                output: self.map_var_id(stmt.output),
            }),
            Statement::Snapshot(stmt) => Statement::Snapshot(StatementSnapshot {
                input: self.map_var_id(stmt.input),
                output_original: self.map_var_id(stmt.output_original),
                output_snapshot: self.map_var_id(stmt.output_snapshot),
            }),
            Statement::Desnap(stmt) => Statement::Desnap(StatementDesnap {
                input: self.map_var_id(stmt.input),
                output: self.map_var_id(stmt.output),
            }),
        };
        self.transform_statement(&mut statement);
        statement
    }

    /// Apply map_var_id to all the variable in the `remapping`.
    fn rebuild_remapping(&mut self, remapping: &VarRemapping) -> VarRemapping {
        let mut remapping = VarRemapping {
            remapping: OrderedHashMap::from_iter(
                remapping.iter().map(|(dst, src)| (self.map_var_id(*dst), self.map_var_id(*src))),
            ),
        };
        self.transform_remapping(&mut remapping);
        remapping
    }

    /// Rebuilds the block end with renamed var and block ids.
    fn rebuild_end(&mut self, end: &FlatBlockEnd) -> FlatBlockEnd {
        let mut end = match end {
            FlatBlockEnd::Return(returns) => FlatBlockEnd::Return(
                returns.iter().map(|var_id| self.map_var_id(*var_id)).collect(),
            ),
            FlatBlockEnd::Fallthrough(block_id, remapping) => FlatBlockEnd::Fallthrough(
                self.map_block_id(*block_id),
                self.rebuild_remapping(remapping),
            ),
            FlatBlockEnd::Goto(block_id, remapping) => {
                FlatBlockEnd::Goto(self.map_block_id(*block_id), self.rebuild_remapping(remapping))
            }
            FlatBlockEnd::NotSet => unreachable!(),
            FlatBlockEnd::Match { info } => FlatBlockEnd::Match {
                info: match info {
                    MatchInfo::Extern(stmt) => MatchInfo::Extern(MatchExternInfo {
                        function: stmt.function,
                        inputs: stmt.inputs.iter().map(|v| self.map_var_id(*v)).collect(),
                        arms: stmt
                            .arms
                            .iter()
                            .map(|(concrete_variant, block_id)| {
                                (concrete_variant.clone(), self.map_block_id(*block_id))
                            })
                            .collect(),
                        location: stmt.location,
                    }),
                    MatchInfo::Enum(stmt) => MatchInfo::Enum(MatchEnumInfo {
                        concrete_enum_id: stmt.concrete_enum_id,
                        input: self.map_var_id(stmt.input),
                        arms: stmt
                            .arms
                            .iter()
                            .map(|(concrete_variant, block_id)| {
                                (concrete_variant.clone(), self.map_block_id(*block_id))
                            })
                            .collect(),
                    }),
                },
            },
        };
        self.transform_end(&mut end);
        end
    }

    /// Rebuilds the block with renamed var and block ids.
    fn rebuild_block(&mut self, block: &FlatBlock) -> FlatBlock {
        let inputs = block.inputs.iter().map(|v| self.map_var_id(*v)).collect();
        let mut statements = vec![];
        for stmt in &block.statements {
            statements.push(self.rebuild_statement(stmt));
        }
        let end = self.rebuild_end(&block.end);
        let mut block = FlatBlock { inputs, statements, end };
        self.transform_block(&mut block);
        block
    }
}

impl<T: Rebuilder> RebuilderEx for T {}
