use cairo_lang_debug::DebugWithDb;
use cairo_lang_debug::debug::DebugWithDbOverride;
use cairo_lang_defs::ids::NamedLanguageElementId;
use itertools::Itertools;
use salsa::Database;

use crate::objects::{
    MatchExternInfo, Statement, StatementCall, StatementConst, StatementStructDestructure,
    VariableId,
};
use crate::{
    Block, BlockEnd, Lowered, MatchArm, MatchEnumInfo, MatchEnumValue, MatchInfo, StatementDesnap,
    StatementEnumConstruct, StatementSnapshot, StatementStructConstruct, VarRemapping, VarUsage,
    VariableArena,
};

/// Holds all the information needed for formatting lowered representations.
/// Acts like a "db" for DebugWithDb.
pub struct LoweredFormatter<'db> {
    pub db: &'db dyn Database,
    pub variables: &'db VariableArena<'db>,
    pub include_usage_location: bool,
}
impl<'db> LoweredFormatter<'db> {
    pub fn new(db: &'db dyn Database, variables: &'db VariableArena<'db>) -> Self {
        Self { db, variables, include_usage_location: false }
    }
}

impl<'db> DebugWithDb<'db> for VarRemapping<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _ctx: &Self::Db) -> std::fmt::Result {
        let mut remapping = self.iter().peekable();
        write!(f, "{{")?;
        while let Some((dst, src)) = remapping.next() {
            write!(f, "v{:?} -> v{:?}", src.var_id.index(), dst.index())?;
            if remapping.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")?;
        Ok(())
    }
}
impl<'db> DebugWithDb<'db> for Lowered<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
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
            write!(f, "{root_block_id:?}")?;
            writeln!(f, " (root):")?;
            root_block.fmt(f, ctx)?;
            writeln!(f)?;
        }
        for (block_id, block) in blocks {
            write!(f, "{block_id:?}")?;
            writeln!(f, ":")?;
            block.fmt(f, ctx)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<'db> DebugWithDb<'db> for Block<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
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

impl<'db> DebugWithDb<'db> for BlockEnd<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
        let outputs = match &self {
            BlockEnd::Return(returns, _location) => {
                write!(f, "  Return(")?;
                returns.iter().map(|var_usage| var_usage.var_id).collect()
            }
            BlockEnd::Panic(data) => {
                write!(f, "  Panic(")?;
                vec![data.var_id]
            }
            BlockEnd::Goto(block_id, remapping) => {
                return write!(f, "  Goto({block_id:?}, {:?})", remapping.debug(ctx));
            }
            BlockEnd::NotSet => return write!(f, "  Not set"),
            BlockEnd::Match { info } => {
                return write!(f, "  Match({:?})", info.debug(ctx));
            }
        };
        let mut outputs = outputs.iter().peekable();
        while let Some(var) = outputs.next() {
            write!(f, "v{:?}", var.index())?;
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
    write!(f, "v{:?}", var_id.index())?;
    let var = &ctx.variables[var_id];
    write!(f, ": {}", var.ty.format(ctx.db))
}

impl<'db> DebugWithDb<'db> for VarUsage<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
        write!(f, "v{:?}", self.var_id.index(),)?;
        if ctx.include_usage_location {
            write!(
                f,
                "{{`{}`}}",
                self.location
                    .long(ctx.db)
                    .stable_location
                    .syntax_node(ctx.db)
                    .get_text_without_trivia(ctx.db)
                    .long(ctx.db)
                    .lines()
                    .map(|s| s.trim())
                    .join(" ")
            )?;
        }
        Ok(())
    }
}

impl<'db> DebugWithDbOverride<'db, LoweredFormatter<'db>> for VariableId {
    fn fmt_override(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _db: &'db LoweredFormatter<'db>,
    ) -> std::fmt::Result {
        write!(f, "v{:?}", self.index())
    }
}

impl<'db> DebugWithDb<'db> for Statement<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
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

impl<'db> DebugWithDb<'db> for MatchInfo<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
        match self {
            MatchInfo::Extern(s) => s.fmt(f, ctx),
            MatchInfo::Enum(s) => s.fmt(f, ctx),
            MatchInfo::Value(s) => s.fmt(f, ctx),
        }
    }
}

impl<'db> DebugWithDb<'db> for StatementConst<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
        self.value.fmt(f, ctx.db)?;
        if self.boxed {
            write!(f, ".into_box()")?
        }
        Ok(())
    }
}

impl<'db> DebugWithDb<'db> for StatementCall<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
        write!(f, "{:?}(", self.function.long(ctx.db).debug(ctx.db))?;
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

impl<'db> DebugWithDb<'db> for MatchExternInfo<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
        write!(f, "match {:?}(", self.function.long(ctx.db).debug(ctx.db))?;
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

impl<'db> DebugWithDb<'db> for MatchArm<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
        write!(f, "    {:?}", self.arm_selector.debug(ctx.db))?;

        if !self.var_ids.is_empty() {
            write!(f, "(")?;
            let mut var_ids = self.var_ids.iter().peekable();
            while let Some(var_id) = var_ids.next() {
                write!(f, "v{:?}", var_id.index())?;
                if var_ids.peek().is_some() {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")")?;
        }

        write!(f, " => {:?},", self.block_id)
    }
}

impl<'db> DebugWithDb<'db> for MatchEnumInfo<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
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

impl<'db> DebugWithDb<'db> for MatchEnumValue<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
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

impl<'db> DebugWithDb<'db> for StatementEnumConstruct<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
        let enum_name = self.variant.concrete_enum_id.enum_id(ctx.db).name(ctx.db).long(ctx.db);
        let variant_name = self.variant.id.name(ctx.db).long(ctx.db);
        write!(f, "{enum_name}::{variant_name}(",)?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

impl<'db> DebugWithDb<'db> for StatementStructConstruct<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
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

impl<'db> DebugWithDb<'db> for StatementStructDestructure<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
        write!(f, "struct_destructure(")?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

impl<'db> DebugWithDb<'db> for StatementSnapshot<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
        write!(f, "snapshot(")?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}

impl<'db> DebugWithDb<'db> for StatementDesnap<'db> {
    type Db = LoweredFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, ctx: &Self::Db) -> std::fmt::Result {
        write!(f, "desnap(")?;
        self.input.fmt(f, ctx)?;
        write!(f, ")")
    }
}
