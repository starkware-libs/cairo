use std::collections::{HashMap, HashSet};

use diagnostics::{Maybe, ToMaybe};
use itertools::chain;
use semantic::corelib::unit_ty;

use super::context::{LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult};
use crate::{Block, BlockEnd, BlockId, Remapping, Statement, VariableId};

pub mod generators;

/// Scope of a block, describing its current state.
/// Maintains the liveness state of lowered variables.
/// Also maintains bound semantic variables. See [SemanticVariablesMap].
// Note: The derive(Default) is for using borrow_as_box below, but it is undesirable for the user to
// create an instance of BlockScope.
#[derive(Default)]
pub struct BlockScope {
    /// Variables given as inputs to the block, including implicits. Relevant for function blocks /
    /// match arm blocks, etc...
    inputs: Vec<VariableId>,
    /// A store for implicit variables, owning their OwnedVariable instances.
    implicits: HashMap<semantic::TypeId, VariableId>,
    // The implicits that are used/changed in this block.
    changed_implicits: HashSet<semantic::TypeId>,
    /// A store for semantic variables, owning their OwnedVariable instances.
    semantics: HashMap<semantic::VarId, VariableId>,
    // The semantic variables that are used/changed in this block.
    changed_semantic: HashSet<semantic::VarId>,
    /// Current sequence of lowered statements emitted.
    statements: Vec<Statement>,
}

impl BlockScope {
    pub fn subscope(&self) -> BlockScope {
        BlockScope {
            inputs: vec![],
            implicits: self.implicits.clone(),
            changed_implicits: Default::default(),
            semantics: self.semantics.clone(),
            changed_semantic: Default::default(),
            statements: Default::default(),
        }
    }

    pub fn add_input(&mut self, ctx: &mut LoweringContext<'_>, ty: semantic::TypeId) -> VariableId {
        let var_id = ctx.new_var(ty);
        self.inputs.push(var_id);
        var_id
    }

    pub fn put_implicit(&mut self, ty: semantic::TypeId, var: VariableId) {
        self.implicits.insert(ty, var);
        self.changed_implicits.insert(ty);
    }

    pub fn get_implicit(&mut self, ty: semantic::TypeId) -> Option<VariableId> {
        self.implicits.get(&ty).copied()
    }

    pub fn put_semantic(&mut self, semantic_var_id: semantic::VarId, var: VariableId) {
        if self.semantics.insert(semantic_var_id, var).is_some() {
            self.changed_semantic.insert(semantic_var_id);
        }
    }

    pub fn get_semantic(&mut self, semantic_var_id: semantic::VarId) -> Option<VariableId> {
        self.semantics.get(&semantic_var_id).copied()
    }

    pub fn unreachable(self) -> Block {
        self.finalize(BlockEnd::Unreachable)
    }

    pub fn panic(self, data_var: VariableId) -> Block {
        self.finalize(BlockEnd::Panic(data_var))
    }

    pub fn goto_callsite(self, expr: Option<VariableId>) -> BlockSealed {
        BlockSealed::GotoCallsite { scope: self, expr }
    }

    pub fn push_statement(&mut self, statement: Statement) {
        self.statements.push(statement)
    }

    pub fn ret(self, ctx: &mut LoweringContext<'_>, expr: VariableId) -> Maybe<Block> {
        let implicit_vars = ctx
            .implicits
            .iter()
            .map(|ty| self.implicits.get(ty).copied())
            .collect::<Option<Vec<_>>>()
            .to_maybe()?;

        let ref_vars = ctx
            .ref_params
            .iter()
            .map(|semantic_var_id| self.semantics.get(semantic_var_id).copied())
            .collect::<Option<Vec<_>>>()
            .to_maybe()?;

        Ok(self.finalize(BlockEnd::Return(chain!(implicit_vars, ref_vars, [expr]).collect())))
    }

    pub fn seal(
        self,
        ctx: &mut LoweringContext<'_>,
        scope_end: BlockScopeEnd,
    ) -> Maybe<BlockSealed> {
        match scope_end {
            BlockScopeEnd::Callsite(expr) => Ok(self.goto_callsite(expr)),
            BlockScopeEnd::Return(expr) => self.ret(ctx, expr).map(From::from),
            BlockScopeEnd::Panic(data_var) => Ok(self.panic(data_var).into()),
            BlockScopeEnd::Unreachable => Ok(self.unreachable().into()),
        }
    }

    fn finalize(self, end: BlockEnd) -> Block {
        Block { inputs: self.inputs, statements: self.statements, end }
    }
}

pub enum BlockScopeEnd {
    Callsite(Option<VariableId>),
    Return(VariableId),
    Panic(VariableId),
    Unreachable,
}

#[derive(Default)]
pub struct InnerRemapping {
    expr: Option<VariableId>,
    implicits: HashMap<semantic::TypeId, VariableId>,
    semantics: HashMap<semantic::VarId, VariableId>,
}

pub enum BlockSealed {
    GotoCallsite { scope: BlockScope, expr: Option<VariableId> },
    Ends(Block),
}
impl BlockSealed {
    fn finalize(self, ctx: &mut LoweringContext<'_>, total_remapping: &InnerRemapping) -> Block {
        match self {
            BlockSealed::GotoCallsite { mut scope, expr } => {
                let mut remapping = Remapping::default();
                if let Some(remapped_var) = total_remapping.expr {
                    let expr = expr.unwrap_or_else(|| {
                        generators::StructConstruct { inputs: vec![], ty: unit_ty(ctx.db.upcast()) }
                            .add(ctx, &mut scope)
                    });

                    remapping.remapping.insert(expr, remapped_var);
                }
                for (ty, remapped_var) in total_remapping.implicits.iter() {
                    remapping.remapping.insert(scope.implicits[ty], *remapped_var);
                }
                for (semantic, remapped_var) in total_remapping.semantics.iter() {
                    remapping.remapping.insert(scope.semantics[semantic], *remapped_var);
                }

                Block {
                    inputs: scope.inputs,
                    statements: scope.statements,
                    end: BlockEnd::Callsite(remapping),
                }
            }
            BlockSealed::Ends(block) => block,
        }
    }
}
impl From<Block> for BlockSealed {
    fn from(b: Block) -> Self {
        BlockSealed::Ends(b)
    }
}

pub struct MergedBlocks {
    pub expr: LoweringResult<LoweredExpr>,
    pub blocks: Vec<BlockId>,
}

pub fn merge_sealed(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    sealed: Vec<BlockSealed>,
) -> MergedBlocks {
    // TODO: add goto for callsite target.
    // TODO: If only one branch has something, dont remap.

    let mut inner_remapping = InnerRemapping::default();
    let mut merged_expr_var: Option<VariableId> = None;
    let mut reachables: usize = 0;
    for s in &sealed {
        let BlockSealed::GotoCallsite { .. } = s else {continue;};
        reachables += 1;
    }

    // Remap only if we have multiple converging branches.
    match reachables {
        1 => {
            for s in &sealed {
                let BlockSealed::GotoCallsite { expr, .. } = s else {continue;};
                merged_expr_var = *expr;
            }
        }
        2.. => {
            for s in &sealed {
                let BlockSealed::GotoCallsite { scope: subscope, expr } = s else {continue;};
                if let Some(var) = expr {
                    if inner_remapping.expr.is_none() {
                        let var = ctx.variables[*var].clone();
                        inner_remapping.expr = Some(ctx.variables.alloc(var));
                        merged_expr_var = inner_remapping.expr;
                    }
                }
                for semantic in &subscope.changed_semantic {
                    inner_remapping.semantics.entry(*semantic).or_insert_with(|| {
                        let var = ctx.variables[subscope.semantics[semantic]].clone();
                        ctx.variables.alloc(var)
                    });
                }
                for implicit in &subscope.changed_implicits {
                    inner_remapping.implicits.entry(*implicit).or_insert_with(|| {
                        let var = ctx.variables[subscope.implicits[implicit]].clone();
                        ctx.variables.alloc(var)
                    });
                }
            }
        }
        _ => {}
    }

    let blocks = sealed
        .into_iter()
        .map(|s| {
            let var = s.finalize(ctx, &inner_remapping);
            ctx.blocks.alloc(var)
        })
        .collect();

    // Apply remapping on scope.
    for (implicit, var) in inner_remapping.implicits {
        scope.put_implicit(implicit, var);
    }
    for (semantic, var) in inner_remapping.semantics {
        scope.put_semantic(semantic, var);
    }

    let expr = match merged_expr_var {
        Some(var) => Ok(LoweredExpr::AtVariable(var)),
        None if reachables > 0 => Ok(LoweredExpr::Tuple(vec![])),
        _ => Err(LoweringFlowError::Unreachable),
    };
    MergedBlocks { expr, blocks }
}
