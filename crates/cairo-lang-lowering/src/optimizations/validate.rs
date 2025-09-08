use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use crate::borrow_check::analysis::StatementLocation;
use crate::{BlockId, Lowered, VariableId};

/// Possible failing validations.
#[derive(Debug)]
pub enum ValidationError {
    /// A variable was used in statement before being introduced.
    UnknownUsageInStatement(VariableId, StatementLocation),
    /// A variable was used in the end of a block before being introduced.
    UnknownUsageInEnd(VariableId, BlockId),
    /// A variable was introduced twice.
    DoubleIntroduction(VariableId, Introduction, Introduction),
}
impl ValidationError {
    pub fn to_message(&self) -> String {
        match self {
            ValidationError::UnknownUsageInStatement(var_id, (block_id, idx)) => format!(
                "Variable `v{}` used in block{}:{idx} before being introduced",
                var_id.index(),
                block_id.0
            ),
            ValidationError::UnknownUsageInEnd(var_id, block_id) => format!(
                "Variable `v{}` used in end of block{} before being introduced",
                var_id.index(),
                block_id.0
            ),
            ValidationError::DoubleIntroduction(var_id, intro1, intro2) => format!(
                "Variable `v{}` introduced twice at: `{intro1}` and `{intro2}`",
                var_id.index()
            ),
        }
    }
}

/// Validates that the lowering structure is valid.
///
/// Currently only does basic SSA validations.
pub fn validate(lowered: &Lowered<'_>) -> Result<(), ValidationError> {
    if lowered.blocks.is_empty() {
        return Ok(());
    }
    let mut introductions = UnorderedHashMap::<VariableId, Introduction>::default();
    for param in &lowered.parameters {
        introductions.insert(*param, Introduction::Parameter);
    }

    let mut stack = vec![BlockId::root()];
    let mut visited = vec![false; lowered.blocks.len()];
    while let Some(block_id) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;
        let block = &lowered.blocks[block_id];
        for (stmt_index, stmt) in block.statements.iter().enumerate() {
            for input in stmt.inputs() {
                if !introductions.contains_key(&input.var_id) {
                    return Err(ValidationError::UnknownUsageInStatement(
                        input.var_id,
                        (block_id, stmt_index),
                    ));
                }
            }
            for output in stmt.outputs().iter() {
                let intro = Introduction::Statement((block_id, stmt_index));
                if let Some(prev) = introductions.insert(*output, intro) {
                    return Err(ValidationError::DoubleIntroduction(*output, prev, intro));
                }
            }
        }
        match &block.end {
            crate::BlockEnd::NotSet => unreachable!(),
            crate::BlockEnd::Return(vars, ..) => {
                for var in vars {
                    if !introductions.contains_key(&var.var_id) {
                        return Err(ValidationError::UnknownUsageInEnd(var.var_id, block_id));
                    }
                }
            }
            crate::BlockEnd::Panic(var) => {
                if !introductions.contains_key(&var.var_id) {
                    return Err(ValidationError::UnknownUsageInEnd(var.var_id, block_id));
                }
            }
            crate::BlockEnd::Goto(target_block_id, remapping) => {
                for (new_var, old_var) in remapping.iter() {
                    if !introductions.contains_key(&old_var.var_id) {
                        return Err(ValidationError::UnknownUsageInEnd(old_var.var_id, block_id));
                    }
                    let intro = Introduction::Remapping(block_id, *target_block_id);
                    if let Some(prev) = introductions.insert(*new_var, intro)
                        && !matches!(
                            prev,
                            Introduction::Remapping(_, target) if target == *target_block_id,
                        )
                    {
                        return Err(ValidationError::DoubleIntroduction(*new_var, prev, intro));
                    }
                }
                stack.push(*target_block_id);
            }
            crate::BlockEnd::Match { info } => {
                for input in info.inputs() {
                    if !introductions.contains_key(&input.var_id) {
                        return Err(ValidationError::UnknownUsageInEnd(input.var_id, block_id));
                    }
                }
                for (arm_idx, arm) in info.arms().iter().enumerate() {
                    for output in arm.var_ids.iter() {
                        let intro = Introduction::Match(block_id, arm_idx);
                        if let Some(prev) = introductions.insert(*output, intro) {
                            return Err(ValidationError::DoubleIntroduction(*output, prev, intro));
                        }
                    }
                    stack.push(arm.block_id);
                }
            }
        }
    }
    Ok(())
}

/// The point a variable was introduced.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Introduction {
    /// The variable is a parameter.
    Parameter,
    /// The variable was introduced by a statement.
    Statement(StatementLocation),
    /// The variable was introduced by a remapping at the end of a block `0` to be supplied to block
    /// `1`.
    Remapping(BlockId, BlockId),
    /// The variable was introduced by a match arm.
    Match(BlockId, usize),
}
impl std::fmt::Display for Introduction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Introduction::Parameter => write!(f, "parameter"),
            Introduction::Statement((block_id, idx)) => write!(f, "block{}:{}", block_id.0, idx),
            Introduction::Remapping(src, dst) => write!(f, "remapping {} -> {}", src.0, dst.0),
            Introduction::Match(block_id, arm_idx) => {
                write!(f, "block{} arm{}", block_id.0, arm_idx)
            }
        }
    }
}
