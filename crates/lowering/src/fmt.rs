use debug::DebugWithDb;
use semantic::db::SemanticGroup;
use semantic::ConcreteVariant;

use crate::lower::Lowered;
use crate::objects::{
    Block, BlockEnd, BlockId, Statement, StatementCall, StatementCallBlock, StatementLiteral,
    StatementMatchExtern, StatementTupleDestruct, VariableId,
};
use crate::{StatementEnumConstruct, StatementMatchEnum, StatementTupleConstruct};

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
        write!(f, "Inputs:")?;
        let mut inputs = self.inputs.iter().peekable();
        while let Some(var) = inputs.next() {
            write!(f, " ")?;
            format_var_with_ty(*var, f, ctx)?;
            if inputs.peek().is_some() {
                write!(f, ",")?;
            }
        }

        writeln!(f, "\nStatements:")?;
        for stmt in &self.statements {
            write!(f, "  ")?;
            stmt.fmt(f, ctx)?;
            writeln!(f)?;
        }

        write!(f, "Drops:")?;
        let mut drops = self.drops.iter().peekable();
        if drops.peek().is_some() {
            write!(f, " ")?;
        }
        while let Some(var) = drops.next() {
            var.fmt(f, ctx)?;
            if drops.peek().is_some() {
                write!(f, ", ")?;
            }
        }

        writeln!(f, "\nEnd:")?;
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
        let mut outputs = outputs.iter().peekable();
        while let Some(var) = outputs.next() {
            var.fmt(f, ctx)?;
            if outputs.peek().is_some() {
                write!(f, ", ")?;
            }
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
        let mut outputs = self.outputs().into_iter().peekable();
        while let Some(var) = outputs.next() {
            format_var_with_ty(var, f, ctx)?;
            if outputs.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, ") <- ")?;
        match self {
            Statement::Literal(stmt) => stmt.fmt(f, ctx),
            Statement::Call(stmt) => stmt.fmt(f, ctx),
            Statement::CallBlock(stmt) => stmt.fmt(f, ctx),
            Statement::MatchExtern(stmt) => stmt.fmt(f, ctx),
            Statement::StructConstruct => todo!(),
            Statement::StructDestruct => todo!(),
            Statement::EnumConstruct(stmt) => stmt.fmt(f, ctx),
            Statement::MatchEnum(stmt) => stmt.fmt(f, ctx),
            Statement::TupleConstruct(stmt) => stmt.fmt(f, ctx),
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
        let mut inputs = self.inputs.iter().peekable();
        while let Some(var) = inputs.next() {
            var.fmt(f, ctx)?;
            if inputs.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementCallBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}()", self.block.debug(ctx))
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementMatchExtern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
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

impl DebugWithDb<LoweredFormatter<'_>> for ConcreteVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let enum_name = self.concrete_enum_id.enum_id(ctx.db).name(ctx.db.upcast());
        let variant_name = self.id.name(ctx.db.upcast());
        write!(f, "{}::{}", enum_name, variant_name)
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementMatchEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "match_enum(")?;
        self.input.fmt(f, ctx)?;
        writeln!(f, ") {{")?;
        for (variant, block) in &self.arms {
            writeln!(f, "    {:?} => {:?},", variant.debug(ctx), block.debug(ctx))?;
        }
        write!(f, "  }}")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementEnumConstruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let enum_name = self.variant.concrete_enum_id.enum_id(ctx.db).name(ctx.db.upcast());
        let variant_name = self.variant.id.name(ctx.db.upcast());
        write!(f, "{enum_name}::{variant_name}(",)?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementTupleConstruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "tuple_construct(")?;
        let mut inputs = self.inputs.iter().peekable();
        while let Some(var) = inputs.next() {
            var.fmt(f, ctx)?;
            if inputs.peek().is_some() {
                write!(f, ", ")?;
            }
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
