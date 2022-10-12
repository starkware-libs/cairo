use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use itertools::chain;
use utils::ordered_hash_map::OrderedHashMap;
use utils::ordered_hash_set::OrderedHashSet;

use super::context::LoweringContext;
use super::semantic_map::{SemanticVariableEntry, SemanticVariablesMap};
use crate::{Block, BlockEnd, Statement, VariableId};

pub mod generators;

/// Wrapper around VariableId, guaranteeing that the variable is alive.
/// Thus, it does not implement copy nor clone.
pub struct OwnedVariable(VariableId);
impl OwnedVariable {
    /// Duplicates the variable if it is duplicatable.
    pub fn try_duplicate(&self, ctx: &LoweringContext<'_>) -> Option<Self> {
        if ctx.variables[self.0].duplicatable { Some(OwnedVariable(self.0)) } else { None }
    }
}

/// Scope of a block, describing its current state.
/// Maintains the liveness state of lowered variables.
/// Also maintains bound semantic variables. See [SemanticVariablesMap].
#[derive(Default)]
pub struct BlockScope {
    /// Variables given as inputs. Relevant for function blocks / match arm blocks, etc...
    inputs: Vec<VariableId>,
    /// Responsible for pulling from outer scopes. See [PullUnifier]. Exists for every non-root
    /// block.
    pull_unifier: Option<Rc<RefCell<PullUnifier>>>,
    /// Semantic variables pulled from higher scopes, to be used as inputs to the block.
    pulled_semantic_vars: OrderedHashMap<semantic::VarId, VariableId>,
    /// Living variables owned by this scope.
    living_variables: OrderedHashSet<VariableId>,
    /// A store for semantic variables, owning their OwnedVariable instances.
    semantic_variables: SemanticVariablesMap,
    /// Current sequence of lowered statements emitted.
    statements: Vec<Statement>,
}

/// Represents how a block ends.
pub enum BlockScopeEnd {
    Callsite(Option<OwnedVariable>),
    Return(Vec<OwnedVariable>),
    Unreachable,
}

impl BlockScope {
    pub fn new_root(
        ctx: &mut LoweringContext<'_>,
        initial_semantic_var_ids: &[semantic::VarId],
    ) -> Self {
        let mut scope = Self::default();
        for semantic_var_id in initial_semantic_var_ids {
            let ty = ctx.semantic_defs[*semantic_var_id].ty();
            let var = scope.introduce_variable(ctx, ty);
            scope.inputs.push(var.0);
            scope.semantic_variables.put(*semantic_var_id, var);
        }
        scope
    }

    pub fn new_subscope(pull_unifier: Rc<RefCell<PullUnifier>>) -> Self {
        Self { pull_unifier: Some(pull_unifier), ..BlockScope::default() }
    }

    /// Puts a semantic variable and its owned lowered variable into the current scope.
    pub fn put_semantic_variable(&mut self, semantic_var_id: semantic::VarId, var: OwnedVariable) {
        self.semantic_variables.put(semantic_var_id, var);
    }

    /// Returns the stored semantic variable if it exists in the scope. Otherwise, introduces it as
    /// an input and returns it.
    /// This can be read as "borrowing" the semantic variable from an outer scope.
    pub fn get_or_pull_semantic_variable(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
    ) -> SemanticVariableEntry {
        self.semantic_variables.get(ctx, semantic_var_id).unwrap_or_else(|| {
            self.pull_semantic_variable(ctx, semantic_var_id)
                .expect(
                    "Requested a non available variable. Semantic model should have caught this.",
                )
                .get_var(ctx)
        })
    }

    /// Seals a BlockScope from adding statements or variables. A sealed block should be finalized
    /// with final pulls to get a [Block]. See [BlockSealed].
    pub fn seal(mut self, end: BlockScopeEnd) -> BlockSealed {
        let end = match end {
            BlockScopeEnd::Callsite(maybe_output) => {
                BlockSealedEnd::Callsite(maybe_output.map(|var| self.take_var(var)))
            }
            BlockScopeEnd::Return(returns) => {
                BlockSealedEnd::Return(returns.into_iter().map(|var| self.take_var(var)).collect())
            }
            BlockScopeEnd::Unreachable => BlockSealedEnd::Unreachable,
        };
        let sealed = BlockSealed {
            inputs: self.inputs,
            pulled_semantic_vars: self.pulled_semantic_vars,
            living_variables: self.living_variables,
            semantic_variables: self.semantic_variables,
            statements: self.statements,
            end,
        };
        // TODO(spapini): Make PullUnifier create a block scope accessible only within a closure,
        //   to force him looking at sealed blocks.
        if let Some(unifier) = self.pull_unifier {
            unifier.try_borrow_mut().unwrap().add_block_sealed(&sealed)
        }
        sealed
    }

    /// Internal. Puts a semantic variable to the semantic store, binds it to a new lowered
    /// variable, and marks it as input to the block.
    fn pull_semantic_variable(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
    ) -> Option<&mut SemanticVariableEntry> {
        let var = self.pull_unifier.as_mut()?.borrow_mut().pull(ctx, semantic_var_id)?;

        assert!(
            self.pulled_semantic_vars.insert(semantic_var_id, var.0).is_none(),
            "Semantic variable introduced more than once as input to the block"
        );

        Some(self.semantic_variables.put(semantic_var_id, var))
    }

    /// Internal. Gets a variable, removing from `living_variables` if not duplicatable.
    fn get_var(&mut self, ctx: &LoweringContext<'_>, var: OwnedVariable) -> VariableId {
        let var_id = var.0;
        if ctx.variables[var_id].duplicatable {
            return var_id;
        }
        self.take_var(var)
    }

    /// Internal. Take a variable, removing from `living_variables`.
    fn take_var(&mut self, var: OwnedVariable) -> VariableId {
        let var_id = var.0;
        assert!(self.living_variables.swap_remove(&var_id), "Unexpected dead variable.");
        var_id
    }

    /// Internal. Introduces a new variable into `living_variables`.
    fn introduce_variable(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        ty: semantic::TypeId,
    ) -> OwnedVariable {
        let var_id = ctx.new_variable(ty);
        assert!(self.living_variables.insert(var_id), "Unexpected reintroduced variable.");
        OwnedVariable(var_id)
    }
}

/// A block that was sealed after adding all the statements, just before determining the final
/// inputs.
pub struct BlockSealed {
    inputs: Vec<VariableId>,
    pulled_semantic_vars: OrderedHashMap<semantic::VarId, VariableId>,
    living_variables: OrderedHashSet<VariableId>,
    semantic_variables: SemanticVariablesMap,
    statements: Vec<Statement>,
    end: BlockSealedEnd,
}

pub enum BlockSealedEnd {
    Callsite(Option<VariableId>),
    Return(Vec<VariableId>),
    Unreachable,
}

impl BlockSealed {
    // TODO(spapini): Add the functions:
    //   pub fn pulls_lower_bound(&self) -> OrderedHashSet<semantic::VarId>
    //   pub fn pushes_upper_bound(&self) -> OrderedHashSet<semantic::VarId>
    /// Finalizes a sealed block. Expected the final sequence of pulls and pushes.
    /// Pulls are all the semantic variables taken from outer scopes (including function params,
    /// etc.). These will be the inputs to the block, in this order.
    /// Pushes are all the semantic variables that are expected to be given back to the outer
    /// scope. The rest will be dropped. These will appear in the outputs of the block in case
    /// of a Callsite ending, before the optional extra output of the block (i.e. block value).
    ///
    /// Pulls must include at least all the pulled variables in block.pulled_semantic_vars.
    /// Pushes must include at most all the living semantic variables that were pulled.
    pub fn finalize(
        self,
        ctx: &mut LoweringContext<'_>,
        pull_unifier_finalized: &PullUnifierFinalized,
    ) -> (Block, BlockEndInfo) {
        let BlockSealed {
            inputs,
            pulled_semantic_vars,
            mut living_variables,
            mut semantic_variables,
            statements,
            end,
        } = self;
        // Pull extra semantic variables if necessary.
        for (semantic_var_id, var) in pull_unifier_finalized.pulls.iter() {
            if !pulled_semantic_vars.contains_key(semantic_var_id) {
                living_variables.insert(var.0);
            }
        }
        // Compute drops.
        let (end, end_info) = match end {
            BlockSealedEnd::Callsite(maybe_output) => {
                let pushes: Vec<_> = pull_unifier_finalized
                    .pushes
                    .iter()
                    .map(|semantic_var_id| {
                        // TODO(spapini): Convert to a diagnostic.

                        semantic_variables
                            .take(*semantic_var_id)
                            .expect("finalize() called with dead output semantic variables.")
                            .var()
                            .expect("Value already moved.")
                            .0
                    })
                    .collect();
                let maybe_output_ty = maybe_output.map(|var_id| ctx.variables[var_id].ty);
                let push_tys = pushes.iter().map(|var_id| ctx.variables[*var_id].ty).collect();
                let outputs = chain!(maybe_output.into_iter(), pushes).collect();
                (BlockEnd::Callsite(outputs), BlockEndInfo::Callsite { maybe_output_ty, push_tys })
            }
            BlockSealedEnd::Return(returns) => (BlockEnd::Return(returns), BlockEndInfo::End),
            BlockSealedEnd::Unreachable => (BlockEnd::Unreachable, BlockEndInfo::End),
        };
        // TODO(spapini): Fix this in case of return.
        let drops = living_variables.into_iter().collect();

        (Block { inputs, statements, drops, end }, end_info)
    }
}

/// Describes the structure of the output variables of a finalized block.
pub enum BlockEndInfo {
    /// The block returns to callsite.
    Callsite {
        /// Type for the "block value" output variable if exists.
        maybe_output_ty: Option<semantic::TypeId>,
        /// Types for the push (rebind) output variables, that get bound to semantic variables at
        /// the calling scope.
        push_tys: Vec<semantic::TypeId>,
    },
    /// The block does not return to callsite, and thus, has no outputs.
    End,
}

/// Responsible for synchronizing pulls from outer scopes between sibling branches.
#[derive(Default)]
pub struct PullUnifier {
    parent_scope: Box<BlockScope>,
    pulls: OrderedHashMap<semantic::VarId, OwnedVariable>,
    can_push: Option<HashSet<semantic::VarId>>,
    // TODO(spapini): Optimized pushes by using shouldnt_push.
}
impl PullUnifier {
    /// Creates a new instance of PullUnifier within a limited closure.
    /// This is necessary for lifetime reasons.
    pub fn with<T, F: FnOnce(Rc<RefCell<Self>>) -> T>(
        parent_scope: &mut BlockScope,
        f: F,
    ) -> (T, PullUnifierFinalized) {
        let new_parent_scope = Box::new(std::mem::take(parent_scope));
        let pull_unifier =
            Rc::new(RefCell::new(Self { parent_scope: new_parent_scope, ..Self::default() }));
        let res = f(pull_unifier.clone());
        let unifier = pull_unifier.take();
        let (finalized, returned_scope) = unifier.finalize();
        *parent_scope = *returned_scope;

        (res, finalized)
    }

    /// Pulls a semantic variable from an outer scope.
    pub fn pull(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
    ) -> Option<OwnedVariable> {
        // Try to take ownership from parent scope if the semantic variable is not present.
        if !self.pulls.contains_key(&semantic_var_id) {
            if let Some(var) =
                self.parent_scope.get_or_pull_semantic_variable(ctx, semantic_var_id).var()
            {
                self.pulls.insert(semantic_var_id, var);
            }
        }

        // If we own it, give a copy.
        let var = self.pulls.get(&semantic_var_id)?;
        Some(OwnedVariable(var.0))
    }

    /// Adds a sealed block to the unifier. This will help the unifier decide on the correct
    /// pulls and pushes.
    pub fn add_block_sealed(&mut self, block_sealed: &BlockSealed) {
        // TODO(spapini): Make this prettier.
        if !matches!(block_sealed.end, BlockSealedEnd::Callsite(_)) {
            return;
        }
        let can_push: HashSet<_> = block_sealed.semantic_variables.alive().copied().collect();
        if let Some(some_can_push) = &mut self.can_push {
            *some_can_push = some_can_push.intersection(&can_push).copied().collect();
        } else {
            self.can_push = Some(can_push);
        }
    }

    /// Finalizes the unifier, deciding on the correct pulls and pushes for all the blocks
    /// encountered.
    pub fn finalize(self) -> (PullUnifierFinalized, Box<BlockScope>) {
        let pushes: Vec<_> = self.can_push.unwrap().into_iter().collect();
        // TODO(spapini): Make stable.
        // TODO(spapini): Optimize pushes by maintaining shouldnt_push.
        (PullUnifierFinalized { pulls: self.pulls, pushes }, self.parent_scope)
    }
}

/// Determined pulls and pushes. Generated after calling [`PullUnifier::finalize()`].
#[derive(Default)]
pub struct PullUnifierFinalized {
    pulls: OrderedHashMap<semantic::VarId, OwnedVariable>,
    pub pushes: Vec<semantic::VarId>,
}
