use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::NamedLanguageElementId;
use id_arena::Arena;
use itertools::Itertools;

use crate::db::LoweringGroup;
use crate::objects::{
    BlockId, MatchExternInfo, Statement, StatementCall, StatementConst, StatementStructDestructure,
    VariableId,
};
use crate::{
    FlatBlock, FlatBlockEnd, FlatLowered, MatchArm, MatchEnumInfo, MatchEnumValue, MatchInfo,
    StatementDesnap, StatementEnumConstruct, StatementSnapshot, StatementStructConstruct,
    VarRemapping, VarUsage, Variable,
};

/// Holds all the information needed for formatting lowered representations.
/// Acts like a "db" for DebugWithDb.
pub struct LoweredFormatter<'db> {
    pub db: &'db dyn LoweringGroup,
    pub variables: &'db Arena<Variable>,
    pub include_usage_location: bool,
}
impl<'db> LoweredFormatter<'db> {
    pub fn new(db: &'db dyn LoweringGroup, variables: &'db Arena<Variable>) -> Self {
        Self { db, variables, include_usage_location: false }
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for VarRemapping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        let mut remapping = self.iter().peekable();
        write!(f, "{{")?;
        while let Some((dst, src)) = remapping.next() {
            src.var_id.fmt(f, ctx)?;
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
impl DebugWithDb<LoweredFormatter<'_>> for FlatLowered {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "Parameters:")?;
        let mut inputs = self.parameters.iter().peekable();
        while let Some(var) = inputs.next() {
            write!(f, " ")?;
            format_var_with_ty(*var, f, ctx)?;
            if inputs.peek().is_some() {
                write!(f, ",")?;
            }
        }
        writeln!(f)?;
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
        writeln!(f, "Statements:")?;
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
            FlatBlockEnd::Return(returns, _location) => {
                write!(f, "  Return(")?;
                returns.iter().map(|var_usage| var_usage.var_id).collect()
            }
            FlatBlockEnd::Panic(data) => {
                write!(f, "  Panic(")?;
                vec![data.var_id]
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

impl DebugWithDb<LoweredFormatter<'_>> for VarUsage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "v{:?}", self.var_id.index(),)?;
        if ctx.include_usage_location {
            write!(
                f,
                "{{`{}`}}",
                self.location
                    .get(ctx.db)
                    .stable_location
                    .syntax_node(ctx.db.upcast())
                    .get_text_without_trivia(ctx.db.upcast())
                    .lines()
                    .map(|s| s.trim())
                    .join(" ")
            )?;
        }
        Ok(())
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
        let mut outputs = self.outputs().iter().peekable();
        while let Some(var) = outputs.next() {
            format_var_with_ty(*var, f, ctx)?;
            if outputs.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, ") <- ")?;
        match self {
            Statement::Const(stmt) => stmt.fmt(f, ctx),
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
            MatchInfo::Value(s) => s.fmt(f, ctx),
        }
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementConst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        self.value.fmt(f, ctx.db)
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for StatementCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}(", self.function.lookup(ctx.db).debug(ctx.db))?;
        for (i, var) in self.inputs.iter().enumerate() {
            let is_last = i == self.inputs.len() - 1;
            if is_last && self.with_coupon {
                write!(f, "__coupon__: ")?;
            }
            var.fmt(f, ctx)?;
            if !is_last {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for MatchExternInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "match {:?}(", self.function.lookup(ctx.db).debug(ctx.db))?;
        let mut inputs = self.inputs.iter().peekable();
        while let Some(var) = inputs.next() {
            var.fmt(f, ctx)?;
            if inputs.peek().is_some() {
                write!(f, ", ")?;
            }
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
        write!(f, "    {:?}", self.arm_selector.debug(ctx.db))?;

        if !self.var_ids.is_empty() {
            write!(f, "(")?;
            let mut var_ids = self.var_ids.iter().peekable();
            while let Some(var_id) = var_ids.next() {
                var_id.fmt(f, ctx)?;
                if var_ids.peek().is_some() {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")")?;
        }

        write!(f, " => {:?},", self.block_id.debug(ctx))
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for MatchEnumInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "match_enum(")?;
        self.input.fmt(f, ctx)?;
        writeln!(f, ") {{")?;
        for arm in &self.arms {
            arm.fmt(f, ctx)?;
            writeln!(f)?;
        }
        write!(f, "  }}")
    }
}

impl DebugWithDb<LoweredFormatter<'_>> for MatchEnumValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &LoweredFormatter<'_>) -> std::fmt::Result {
        write!(f, "match_enum.(")?;
        self.input.fmt(f, ctx)?;
        writeln!(f, ") {{")?;
        for arm in &self.arms {
            arm.fmt(f, ctx)?;
            writeln!(f)?;
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
