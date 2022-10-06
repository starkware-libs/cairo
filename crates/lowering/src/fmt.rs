use debug::DebugWithDb;
use semantic::db::SemanticGroup;

use crate::lower::Lowered;
use crate::objects::{
    Block, BlockEnd, BlockId, MatchArm, Statement, StatementCall, StatementCallBlock,
    StatementLiteral, StatementMatchExtern, StatementTupleDestruct, VariableId,
};

/// Holds all the information needed for formatting lowered representations.
/// Acts like a "db" for DebugWithDb.
pub struct LoweredFormatter<'db> {
    pub db: &'db (dyn SemanticGroup + 'static),
    pub lowered: &'db Lowered,
}

impl DebugWithDb<LoweredFormatter<'_>> for Lowered {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        for (block_id, block) in self.blocks.iter() {
            block_id.fmt(f, ctx)?;
            writeln!(f, ":")?;
            block.fmt(f, ctx)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        writeln!(f, "Inputs:")?;
        write!(f, "  ")?;
        for var in &self.inputs {
            format_var_with_ty(*var, f, ctx)?;
            write!(f, ", ")?;
        }
        writeln!(f, "\nStatements:")?;
        for stmt in &self.statements {
            write!(f, "  ")?;
            stmt.fmt(f, ctx)?;
            writeln!(f)?;
        }
        writeln!(f, "End:")?;
        self.end.fmt(f, ctx)?;
        writeln!(f)
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for BlockEnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let outputs = match &self {
            BlockEnd::Callsite(outputs) => {
                write!(f, "  Callsite(")?;
                outputs
            }
            BlockEnd::Return(outputs) => {
                write!(f, "  Return(")?;
                outputs
            }
            BlockEnd::Unreachable => {
                return write!(f, "  Unreachable");
            }
        };
        for var in outputs {
            var.fmt(f, ctx)?;
            write!(f, ", ")?;
        }
        write!(f, ")")
    }
}

fn format_var_with_ty(
    var_id: VariableId,
    f: &mut std::fmt::Formatter<'_>,
    ctx: &LoweredFormatter<'_>,
) -> std::fmt::Result {
    var_id.fmt(f, ctx)?;
    write!(f, ": {}", ctx.lowered.variables[var_id].ty.format(ctx.db))
}

impl DebugWithDb<LoweredFormatter<'_>> for BlockId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _lowered: &LoweredFormatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "blk{:?}", self.index())
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for VariableId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _lowered: &LoweredFormatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "v{:?}", self.index())
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for var in self.outputs() {
            format_var_with_ty(var, f, ctx)?;
            write!(f, ", ")?;
        }
        write!(f, ") <- ")?;
        match self {
            Statement::Literal(stmt) => stmt.fmt(f, ctx),
            Statement::Call(stmt) => stmt.fmt(f, ctx),
            Statement::CallBlock(stmt) => stmt.fmt(f, ctx),
            Statement::MatchExtern(stmt) => stmt.fmt(f, ctx),
            Statement::StructConstruct => todo!(),
            Statement::StructDestruct => todo!(),
            Statement::EnumConstruct => todo!(),
            Statement::MatchEnum => todo!(),
            Statement::TupleConstruct => todo!(),
            Statement::TupleDestruct(stmt) => stmt.fmt(f, ctx),
        }
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementLiteral {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _ctx: &LoweredFormatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "{}u", self.value)
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}(", self.function.debug(ctx.db))?;
        for var in &self.inputs {
            var.fmt(f, ctx)?;
            write!(f, ", ")?;
        }
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementCallBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}(", self.block)?;
        for var in &self.inputs {
            var.fmt(f, ctx)?;
        }
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementTupleDestruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "tuple_destruct(")?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementMatchExtern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        // TODO(spapini): Format function name better.
        write!(f, "match {:?}(", self.function.debug(ctx.db))?;
        for var in &self.inputs {
            var.fmt(f, ctx)?;
        }
        writeln!(f, ") {{")?;
        for arm in &self.arms {
            arm.fmt(f, ctx)?;
            writeln!(f)?;
        }
        write!(f, "  }}")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for MatchArm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "    (")?;
        for var in &self.arm_variables {
            format_var_with_ty(*var, f, ctx)?;
            write!(f, ", ")?;
        }
        write!(f, ") => ")?;
        self.block.fmt(f, ctx)?;
        write!(f, ",")
    }
}
