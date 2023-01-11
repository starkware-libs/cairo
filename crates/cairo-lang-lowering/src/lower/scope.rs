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
    /// Creates a new [BlockScope] for the root block of a function body.
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

    /// Creates a [BlockScope] for a subscope.
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

    /// Creates a [BlockScope] for a subscope with unchanged refs.
    pub fn subscope_with_bound_refs(&self) -> BlockScope {
        let mut subscope = self.subscope();
        subscope.bind_refs();
        subscope
    }

    /// Adds an input to the block.
    pub fn add_input(&mut self, ctx: &mut LoweringContext<'_>, ty: semantic::TypeId) -> VariableId {
        let var_id = ctx.new_var(ty);
        self.inputs.push(var_id);
        var_id
    }

    /// Binds an implicit to a lowered variable.
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

    /// Gets the current lowered variable bound to an implicit.
    pub fn get_implicit(&mut self, ty: semantic::TypeId) -> Option<VariableId> {
        self.implicits.get(&ty).copied()
    }

    /// Binds a semantic variable to a lowered variable.
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

    /// Gets the current lowered variable bound to a semantic variable.
    pub fn get_semantic(&mut self, semantic_var_id: semantic::VarId) -> Option<VariableId> {
        self.semantics.get(&semantic_var_id).copied()
    }

    /// Confirms that all refs (including implicits) for the beginning of the block are set using
    /// put_implicit() and put_semantic(). Should be called once per block before any statement.
    pub fn bind_refs(&mut self) {
        assert!(self.initial_refs.is_none(), "References cannot be bound twice.");
        self.initial_refs = Some(
            self.current_refs.iter().copied().map(|v| v.expect("Reference not bound.")).collect(),
        );
    }

    /// Adds a statement to the block. finalize_statement() should be called after binding the refs
    /// that are relevant right after the statement, and always before the next push_statement().
    pub fn push_statement(&mut self, stmt: Statement) {
        assert!(self.pending_statement.replace(stmt).is_none(), "finalize_statement() not called.");
    }

    /// Finalizes a statement after binding its refs.
    pub fn finalize_statement(&mut self) {
        let statement = self.pending_statement.take().expect("push_statement() not called.");
        self.statements.push(StructuredStatement {
            statement,
            ref_updates: std::mem::take(&mut self.pending_ref_updates),
        });
    }

    /// Adds a statement to the block without rebinding refs - the refs will remain like they were
    /// before this statement.
    pub fn push_finalized_statement(&mut self, stmt: Statement) {
        self.push_statement(stmt);
        self.finalize_statement();
    }

    /// Ends a block with Unreachable.
    pub fn unreachable(self) -> StructuredBlock {
        self.finalize(StructuredBlockEnd::Unreachable)
    }

    /// Ends a block with Panic.
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

    /// Ends a block with Callsite.
    pub fn goto_callsite(self, expr: Option<VariableId>) -> BlockSealed {
        BlockSealed::GotoCallsite { scope: self, expr }
    }

    /// Ends a block with Return.
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

    /// Seals a block. This is meant to end a block when not all the information is necessarily
    /// known yet, e.g. where is the callsite and what are the remapping we need to perform.
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

    /// Ends a block with known ending information. Used by [BlockSealed].
    fn finalize(self, end: StructuredBlockEnd) -> StructuredBlock {
        StructuredBlock {
            initial_refs: self.initial_refs.expect("References have not been bound yet."),
            inputs: self.inputs,
            statements: self.statements,
            end,
        }
    }
}

/// Describes how a block ends, with possibly partial information regarding callsite and remapping.
/// Used for sealing a block.
pub enum BlockScopeEnd {
    Callsite(Option<VariableId>),
    Return(VariableId),
    Panic(VariableId),
    Unreachable,
}

/// Remapping of lowered variables with more semantic information regarding what is the semantic
/// role fo the lowered variables.
#[derive(Default)]
pub struct SemanticRemapping {
    expr: Option<VariableId>,
    implicits: OrderedHashMap<semantic::TypeId, VariableId>,
    semantics: OrderedHashMap<semantic::VarId, VariableId>,
}

/// A sealed BlockScope, ready to be merged with sibling blocks to end a the block.
#[allow(clippy::large_enum_variant)]
pub enum BlockSealed {
    GotoCallsite { scope: BlockScope, expr: Option<VariableId> },
    Ends(StructuredBlock),
}
impl BlockSealed {
    /// Given the extra information needed, ends a sealed block.
    fn finalize(
        self,
        ctx: &mut LoweringContext<'_>,
        total_remapping: &SemanticRemapping,
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

/// Information regarding the merge of sibling blocks.
pub struct MergedBlocks {
    /// The converged expression of the blocks, usable at the calling scope.
    pub expr: LoweringResult<LoweredExpr>,
    /// Merged block ids.
    pub blocks: Vec<BlockId>,
}

/// Merges sibling sealed blocks.
pub fn merge_sealed(
    ctx: &mut LoweringContext<'_>,
    scope: &mut BlockScope,
    sealed: Vec<BlockSealed>,
) -> MergedBlocks {
    // TODO(spapini): When adding Gotos, include the callsite target in the required information to
    // merge.
    // TODO(spapini): Don't remap if we have a single branch.

    let mut inner_remapping = SemanticRemapping::default();
    let mut merged_expr_var: Option<VariableId> = None;
    let mut reachables = 0;

    // Remap only if we have multiple converging branches.
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
