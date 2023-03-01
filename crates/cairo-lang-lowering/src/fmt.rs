use cairo_lang_debug::DebugWithDb;
use cairo_lang_semantic::ConcreteVariant;
use id_arena::Arena;
use itertools::chain;

use crate::db::LoweringGroup;
use crate::objects::{
    BlockId, MatchExternInfo, Statement, StatementCall, StatementLiteral,
    StatementStructDestructure, StructuredBlock, StructuredBlockEnd, VariableId,
};
use crate::{
    FlatBlock, FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchInfo, StatementDesnap,
    StatementEnumConstruct, StatementSnapshot, StatementStructConstruct, StructuredLowered,
    StructuredStatement, VarRemapping, Variable,
};

/// Holds all the information needed for formatting lowered representations.
/// Acts like a "db" for DebugWithDb.
pub struct LoweredFormatter<'db> {
    pub db: &'db (dyn LoweringGroup + 'static),
    pub variables: &'db Arena<Variable>,
}

impl DebugWithDb<LoweredFormatter<'_>> for StructuredLowered {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let mut blocks = self.blocks.iter();
        if let Some((root_block_id, root_block)) = blocks.next() {
            root_block_id.fmt(f, ctx)?;
            writeln!(f, " (root):")?;
            root_block.fmt(f, ctx)?;
            writeln!(f)?;
        }
        for (block_id, block) in blocks {
            block_id.fmt(f, ctx)?;
            writeln!(f, ":")?;
            block.fmt(f, ctx)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StructuredBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        if !self.is_set() {
            return write!(f, "BLOCK_NOT_SET");
        }
        write!(f, "Inputs:")?;
        let mut inputs = self.inputs.iter().peekable();
        while let Some(var) = inputs.next() {
            write!(f, " ")?;
            format_var_with_ty(*var, f, ctx)?;
            if inputs.peek().is_some() {
                write!(f, ",")?;
            }
        }

        write!(f, "\nInitial implicits:")?;
        let mut implicits = self.initial_implicits.iter().peekable();
        while let Some(var) = implicits.next() {
            write!(f, " ")?;
            format_var_with_ty(*var, f, ctx)?;
            if implicits.peek().is_some() {
                write!(f, ",")?;
            }
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

impl DebugWithDb<LoweredFormatter<'_>> for VarRemapping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let mut remapping = self.iter().peekable();
        write!(f, "{{")?;
        while let Some((dst, src)) = remapping.next() {
            src.fmt(f, ctx)?;
            write!(f, " -> ")?;
            dst.fmt(f, ctx)?;
            if remapping.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StructuredBlockEnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let outputs: Vec<VariableId> = match &self {
            StructuredBlockEnd::Goto { target, remapping } => {
                return write!(f, "  Goto({}, {:?})", target.0, remapping.debug(ctx));
            }
            StructuredBlockEnd::Return { implicits, returns } => {
                write!(f, "  Return(")?;
                chain!(implicits, returns).copied().collect()
            }
            StructuredBlockEnd::Panic { implicits, data } => {
                write!(f, "  Panic(")?;
                chain!(implicits, [data]).copied().collect()
            }
            StructuredBlockEnd::NotSet => unreachable!(),
            StructuredBlockEnd::Match { info } => {
                return write!(f, "  Match({:?})", info.debug(ctx));
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

impl DebugWithDb<LoweredFormatter<'_>> for FlatLowered {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let mut blocks = self.blocks.iter();
        if let Some((root_block_id, root_block)) = blocks.next() {
            root_block_id.fmt(f, ctx)?;
            writeln!(f, " (root):")?;
            root_block.fmt(f, ctx)?;
            writeln!(f)?;
        }
        for (block_id, block) in blocks {
            block_id.fmt(f, ctx)?;
            writeln!(f, ":")?;
            block.fmt(f, ctx)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for FlatBlock {
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

        writeln!(f, "End:")?;
        self.end.fmt(f, ctx)?;
        writeln!(f)
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for FlatBlockEnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let outputs = match &self {
            FlatBlockEnd::Return(returns) => {
                write!(f, "  Return(")?;
                returns
            }
            FlatBlockEnd::Fallthrough(block_id, remapping) => {
                return write!(
                    f,
                    "  Fallthrough({:?}, {:?})",
                    block_id.debug(ctx),
                    remapping.debug(ctx)
                );
            }
            FlatBlockEnd::Goto(block_id, remapping) => {
                return write!(f, "  Goto({:?}, {:?})", block_id.debug(ctx), remapping.debug(ctx));
            }
            FlatBlockEnd::NotSet => unreachable!(),
            FlatBlockEnd::Match { info } => {
                return write!(f, "  Match({:?})", info.debug(ctx));
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
    let var = &ctx.variables[var_id];
    write!(f, ": {}", var.ty.format(ctx.db.upcast()))
}

impl DebugWithDb<LoweredFormatter<'_>> for BlockId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _lowered: &LoweredFormatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "blk{:?}", self.0)
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

impl DebugWithDb<LoweredFormatter<'_>> for StructuredStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        self.statement.fmt(f, ctx)?;
        if !self.implicit_updates.is_empty() {
            write!(f, "\n    Ref changes: ")?;
            for (i, (ref_index, var_id)) in self.implicit_updates.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "r{} <- {:?}", ref_index.0, var_id.debug(ctx))?;
            }
        }
        Ok(())
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
            Statement::StructConstruct(stmt) => stmt.fmt(f, ctx),
            Statement::StructDestructure(stmt) => stmt.fmt(f, ctx),
            Statement::EnumConstruct(stmt) => stmt.fmt(f, ctx),
            Statement::Snapshot(stmt) => stmt.fmt(f, ctx),
            Statement::Desnap(stmt) => stmt.fmt(f, ctx),
        }
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for MatchInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        match self {
            MatchInfo::Extern(s) => s.fmt(f, ctx),
            MatchInfo::Enum(s) => s.fmt(f, ctx),
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

impl DebugWithDb<LoweredFormatter<'_>> for MatchExternInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "match {:?}(", self.function.debug(ctx.db))?;
        let mut inputs = self.inputs.iter().peekable();
        while let Some(var) = inputs.next() {
            var.fmt(f, ctx)?;
            if inputs.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        writeln!(f, ") {{")?;
        for (variant, block_id) in &self.arms {
            writeln!(f, "    {:?} => {:?},", variant.debug(ctx), block_id.debug(ctx))?;
        }
        write!(f, "  }}")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for ConcreteVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let enum_name = self.concrete_enum_id.enum_id(ctx.db.upcast()).name(ctx.db.upcast());
        let variant_name = self.id.name(ctx.db.upcast());
        write!(f, "{enum_name}::{variant_name}")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for MatchEnumInfo {
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
        let enum_name =
            self.variant.concrete_enum_id.enum_id(ctx.db.upcast()).name(ctx.db.upcast());
        let variant_name = self.variant.id.name(ctx.db.upcast());
        write!(f, "{enum_name}::{variant_name}(",)?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementStructConstruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "struct_construct(")?;
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

impl DebugWithDb<LoweredFormatter<'_>> for StatementStructDestructure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "struct_destructure(")?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementSnapshot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "snapshot(")?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementDesnap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "desnap(")?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}
