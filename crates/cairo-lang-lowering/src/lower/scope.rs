use cairo_lang_diagnostics::{Maybe, ToMaybe};
use cairo_lang_semantic as semantic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::chain;
use semantic::corelib::unit_ty;

use super::context::{LoweredExpr, LoweringContext, LoweringFlowError, LoweringResult};
use super::generators;
use crate::{
    BlockId, RefIndex, Remapping, Statement, StructuredBlock, StructuredBlockEnd,
    StructuredStatement, VariableId,
};

/// Scope of a block, describing its current state.
/// Maintains the liveness state of lowered variables.
/// Also maintains bound semantic variables. See [SemanticVariablesMap].
// Note: The derive(Default) is for using borrow_as_box below, but it is undesirable for the user to
// create an instance of BlockScope.
pub struct BlockScope {
    /// The variable ids currently bound to the ref variables.
    current_refs: Vec<Option<VariableId>>,
    /// The variable ids bound to the ref variables (including implicits) at the beginning of the
    /// block.
    initial_refs: Option<Vec<VariableId>>,
    /// Variables given as inputs to the block, including implicits. Relevant for function blocks /
    /// match arm blocks, etc...
    inputs: Vec<VariableId>,
    /// A store for implicit variables, owning their OwnedVariable instances.
    implicits: OrderedHashMap<semantic::TypeId, VariableId>,
    // The implicits that are used/changed in this block.
    changed_implicits: OrderedHashSet<semantic::TypeId>,
    /// A store for semantic variables, owning their OwnedVariable instances.
    semantics: OrderedHashMap<semantic::VarId, VariableId>,
    // The semantic variables that are used/changed in this block.
    changed_semantic: OrderedHashSet<semantic::VarId>,
    /// Current sequence of lowered statements emitted.
    statements: Vec<StructuredStatement>,
    /// Statement pending finalize_statement().
    pending_statement: Option<Statement>,
    /// Updates to the variable ids bound to the ref variables (including implicits), from the last
    /// update until exactly after next statement. When finalize_statement() will be called, these
    /// updates will be added to the statement.
    pending_ref_updates: OrderedHashMap<RefIndex, VariableId>,
}

impl BlockScope {
    pub fn root(ctx: &LoweringContext<'_>) -> Self {
        BlockScope {
            current_refs: (0..(ctx.implicits.len() + ctx.ref_params.len())).map(|_| None).collect(),
            initial_refs: None,
            inputs: vec![],
            implicits: Default::default(),
            changed_implicits: Default::default(),
            semantics: Default::default(),
            changed_semantic: Default::default(),
            statements: Default::default(),
            pending_statement: None,
            pending_ref_updates: Default::default(),
        }
    }

    pub fn subscope(&self) -> BlockScope {
        BlockScope {
            current_refs: self.current_refs.clone(),
            initial_refs: None,
            inputs: vec![],
            implicits: self.implicits.clone(),
            changed_implicits: Default::default(),
            semantics: self.semantics.clone(),
            changed_semantic: Default::default(),
            statements: Default::default(),
            pending_statement: None,
            pending_ref_updates: Default::default(),
        }
    }

    pub fn subscope_with_bound_refs(&self) -> BlockScope {
        let mut subscope = self.subscope();
        subscope.bind_refs();
        subscope
    }

    pub fn add_input(&mut self, ctx: &mut LoweringContext<'_>, ty: semantic::TypeId) -> VariableId {
        let var_id = ctx.new_var(ty);
        self.inputs.push(var_id);
        var_id
    }

    pub fn put_implicit(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        ty: semantic::TypeId,
        var: VariableId,
    ) {
        let (implicit_index, _) = ctx
            .implicits
            .iter()
            .enumerate()
            .find(|(_, imp_ty)| **imp_ty == ty)
            .expect("Unknown implicit.");
        self.pending_ref_updates.insert(RefIndex(implicit_index), var);
        self.current_refs[implicit_index] = Some(var);
        self.implicits.insert(ty, var);
        self.changed_implicits.insert(ty);
    }

    pub fn get_implicit(&mut self, ty: semantic::TypeId) -> Option<VariableId> {
        self.implicits.get(&ty).copied()
    }

    pub fn put_semantic(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
        var: VariableId,
    ) {
        if self.semantics.insert(semantic_var_id, var).is_some() {
            self.changed_semantic.insert(semantic_var_id);
        }
        if let Some((ref_index, _)) = ctx
            .ref_params
            .iter()
            .enumerate()
            .find(|(_, ref_semantic_var_id)| **ref_semantic_var_id == semantic_var_id)
        {
            let index = ctx.implicits.len() + ref_index;
            self.pending_ref_updates.insert(RefIndex(index), var);
            self.current_refs[index] = Some(var);
        }
    }

    pub fn get_semantic(&mut self, semantic_var_id: semantic::VarId) -> Option<VariableId> {
        self.semantics.get(&semantic_var_id).copied()
    }

    pub fn bind_refs(&mut self) {
        assert!(self.initial_refs.is_none(), "References cannot be bound twice.");
        self.initial_refs = Some(
            self.current_refs.iter().copied().map(|v| v.expect("Reference not bound.")).collect(),
        );
    }

    pub fn unreachable(self) -> StructuredBlock {
        self.finalize(StructuredBlockEnd::Unreachable)
    }

    pub fn panic(self, ctx: &mut LoweringContext<'_>, data: VariableId) -> Maybe<StructuredBlock> {
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

        Ok(self.finalize(StructuredBlockEnd::Panic {
            refs: chain!(implicit_vars, ref_vars).collect(),
            data,
        }))
    }

    pub fn goto_callsite(self, expr: Option<VariableId>) -> BlockSealed {
        BlockSealed::GotoCallsite { scope: self, expr }
    }

    pub fn push_statement(&mut self, stmt: Statement) {
        assert!(self.pending_statement.replace(stmt).is_none(), "finalize_statement() not called.");
    }

    pub fn finalize_statement(&mut self) {
        let statement = self.pending_statement.take().expect("push_statement() not called.");
        self.statements.push(StructuredStatement {
            statement,
            ref_updates: std::mem::take(&mut self.pending_ref_updates),
        });
    }

    pub fn push_finalized_statement(&mut self, stmt: Statement) {
        self.push_statement(stmt);
        self.finalize_statement();
    }

    pub fn ret(self, ctx: &mut LoweringContext<'_>, expr: VariableId) -> Maybe<StructuredBlock> {
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

        Ok(self.finalize(StructuredBlockEnd::Return {
            refs: chain!(implicit_vars, ref_vars).collect(),
            returns: vec![expr],
        }))
    }

    pub fn seal(
        self,
        ctx: &mut LoweringContext<'_>,
        scope_end: BlockScopeEnd,
    ) -> Maybe<BlockSealed> {
        match scope_end {
            BlockScopeEnd::Callsite(expr) => Ok(self.goto_callsite(expr)),
            BlockScopeEnd::Return(expr) => self.ret(ctx, expr).map(From::from),
            BlockScopeEnd::Panic(data_var) => self.panic(ctx, data_var).map(From::from),
            BlockScopeEnd::Unreachable => Ok(self.unreachable().into()),
        }
    }

    fn finalize(self, end: StructuredBlockEnd) -> StructuredBlock {
        StructuredBlock {
            initial_refs: self.initial_refs.expect("References have not been bound yet."),
            inputs: self.inputs,
            statements: self.statements,
            end,
        }
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
    implicits: OrderedHashMap<semantic::TypeId, VariableId>,
    semantics: OrderedHashMap<semantic::VarId, VariableId>,
}

#[allow(clippy::large_enum_variant)]
pub enum BlockSealed {
    GotoCallsite { scope: BlockScope, expr: Option<VariableId> },
    Ends(StructuredBlock),
}
impl BlockSealed {
    fn finalize(
        self,
        ctx: &mut LoweringContext<'_>,
        total_remapping: &InnerRemapping,
    ) -> StructuredBlock {
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
                    remapping.remapping.insert(scope.implicits[*ty], *remapped_var);
                }
                for (semantic, remapped_var) in total_remapping.semantics.iter() {
                    remapping.remapping.insert(scope.semantics[*semantic], *remapped_var);
                }

                scope.finalize(StructuredBlockEnd::Callsite(remapping))
            }
            BlockSealed::Ends(block) => block,
        }
    }
}
impl From<StructuredBlock> for BlockSealed {
    fn from(b: StructuredBlock) -> Self {
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
    let mut reachables = 0;

    // Remap only if we have multiple converging branches.
    // TODO(spapini): Don't remap if we have a single branch.
    for s in &sealed {
        let BlockSealed::GotoCallsite { scope: subscope, expr } = s else {continue;};
        reachables += 1;
        if let Some(var) = expr {
            if inner_remapping.expr.is_none() {
                let var = ctx.variables[*var].clone();
                inner_remapping.expr = Some(ctx.variables.alloc(var));
                merged_expr_var = inner_remapping.expr;
            }
        }
        for semantic in subscope.changed_semantic.iter() {
            inner_remapping.semantics.entry(*semantic).or_insert_with(|| {
                let var = ctx.variables[subscope.semantics[*semantic]].clone();
                ctx.variables.alloc(var)
            });
        }
        for implicit in subscope.changed_implicits.iter() {
            inner_remapping.implicits.entry(*implicit).or_insert_with(|| {
                let var = ctx.variables[subscope.implicits[*implicit]].clone();
                ctx.variables.alloc(var)
            });
        }
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
        scope.put_implicit(ctx, implicit, var);
    }
    for (semantic, var) in inner_remapping.semantics {
        scope.put_semantic(ctx, semantic, var);
    }

    let expr = match merged_expr_var {
        Some(var) => Ok(LoweredExpr::AtVariable(var)),
        None if reachables > 0 => Ok(LoweredExpr::Tuple(vec![])),
        _ => Err(LoweringFlowError::Unreachable),
    };
    MergedBlocks { expr, blocks }
}
