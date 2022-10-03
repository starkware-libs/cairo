use std::fmt::Display;

use debug::DebugWithDb;
use semantic::db::SemanticGroup;

use crate::lower::Lowered;
use crate::objects::{
    Block, BlockEnd, BlockId, MatchArm, Statement, StatementCallBlock, StatementCallExtern,
    StatementCallUserFunc, StatementLiteral, StatementMatchExtern, StatementTupleDestruct,
    VariableId,
};

pub struct FmtContext<'db> {
    pub db: &'db (dyn SemanticGroup + 'static),
    pub lowered: &'db Lowered,
}
impl<'db> Display for FmtContext<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.lowered.fmt(f, self)?;
        Ok(())
    }
}

pub trait LoweredFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &FmtContext<'_>) -> std::fmt::Result;
}

impl LoweredFormat for Lowered {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &FmtContext<'_>) -> std::fmt::Result {
        for (block_id, block) in self.blocks.iter() {
            block_id.fmt(f, ctx)?;
            writeln!(f, ":")?;
            block.fmt(f, ctx)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl LoweredFormat for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &FmtContext<'_>) -> std::fmt::Result {
        writeln!(f, "Inputs:")?;
        write!(f, "  ")?;
        for var in &self.inputs {
            format_var_with_ty(*var, f, ctx)?;
            writeln!(f, ", ")?;
        }
        writeln!(f, "Statements:")?;
        for stmt in &self.statements {
            write!(f, "  ")?;
            stmt.fmt(f, ctx)?
        }
        writeln!(f, "End:")?;
        self.end.fmt(f, ctx)
    }
}

impl LoweredFormat for BlockEnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &FmtContext<'_>) -> std::fmt::Result {
        let outputs = match &self {
            BlockEnd::Callsite(outputs) => {
                write!(f, "  Callsite(")?;
                outputs
            }
            BlockEnd::Return(outputs) => {
                write!(f, "  Return(")?;
                outputs
            }
        };
        for var in outputs {
            var.fmt(f, ctx)?;
            write!(f, ", ")?;
        }
        writeln!(f, ")")
    }
}

fn format_var_with_ty(
    var_id: VariableId,
    f: &mut std::fmt::Formatter<'_>,
    ctx: &FmtContext<'_>,
) -> std::fmt::Result {
    var_id.fmt(f, ctx)?;
    write!(f, ": {}", ctx.lowered.variables[var_id].ty.format(ctx.db))
}

impl LoweredFormat for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _lowered: &FmtContext<'_>) -> std::fmt::Result {
        write!(f, "blk{:?}", self.index())
    }
}

impl LoweredFormat for VariableId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _lowered: &FmtContext<'_>) -> std::fmt::Result {
        write!(f, "v{:?}", self.index())
    }
}

impl LoweredFormat for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &FmtContext<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for var in self.outputs() {
            format_var_with_ty(var, f, ctx)?;
            write!(f, ", ")?;
        }
        writeln!(f, ") <-")?;
        write!(f, "    ")?;
        match self {
            Statement::Literal(stmt) => stmt.fmt(f, ctx),
            Statement::CallUserFunc(stmt) => stmt.fmt(f, ctx),
            Statement::CallBlock(stmt) => stmt.fmt(f, ctx),
            Statement::CallExtern(stmt) => stmt.fmt(f, ctx),
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

impl LoweredFormat for StatementLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _ctx: &FmtContext<'_>) -> std::fmt::Result {
        writeln!(f, "{}u", self.value)
    }
}

impl LoweredFormat for StatementCallUserFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &FmtContext<'_>) -> std::fmt::Result {
        // TODO(spapini): Format function name better.
        write!(f, "{:?}(", self.function.debug(ctx.db))?;
        for var in &self.inputs {
            var.fmt(f, ctx)?;
        }
        writeln!(f, ")")
    }
}

impl LoweredFormat for StatementCallExtern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &FmtContext<'_>) -> std::fmt::Result {
        // TODO(spapini): Format function name better.
        write!(f, "{:?}(", self.function.debug(ctx.db))?;
        for var in &self.inputs {
            var.fmt(f, ctx)?;
        }
        writeln!(f, ")")
    }
}

impl LoweredFormat for StatementCallBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &FmtContext<'_>) -> std::fmt::Result {
        write!(f, "{:?}(", self.block)?;
        for var in &self.inputs {
            var.fmt(f, ctx)?;
        }
        writeln!(f, ")")
    }
}

impl LoweredFormat for StatementTupleDestruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &FmtContext<'_>) -> std::fmt::Result {
        write!(f, "tuple_destruct(")?;
        self.input.fmt(f, ctx)?;
        writeln!(f, ")")
    }
}

impl LoweredFormat for StatementMatchExtern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &FmtContext<'_>) -> std::fmt::Result {
        // TODO(spapini): Format function name better.
        write!(f, "match {:?}(", self.function.debug(ctx.db))?;
        for var in &self.inputs {
            var.fmt(f, ctx)?;
        }
        writeln!(f, ") {{")?;
        for arm in &self.arms {
            arm.fmt(f, ctx)?;
        }
        writeln!(f, "  }}")
    }
}

impl LoweredFormat for MatchArm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &FmtContext<'_>) -> std::fmt::Result {
        write!(f, "    (")?;
        for var in &self.arm_variables {
            format_var_with_ty(*var, f, ctx)?;
            writeln!(f, ", ")?;
        }
        write!(f, ") => ")?;
        self.block.fmt(f, ctx)?;
        writeln!(f, ",")
    }
}
