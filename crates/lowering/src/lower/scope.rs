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
        self.semantic_variables
            .get(ctx, semantic_var_id)
            .unwrap_or_else(|| self.pull_semantic_variable(ctx, semantic_var_id).get_var(ctx))
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
        BlockSealed { block: self, end }
    }

    /// Internal. Puts a semantic variable to the semantic store, binds it to a new lowered
    /// variable, and marks it as input to the block.
    fn pull_semantic_variable(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
    ) -> &mut SemanticVariableEntry {
        let ty = ctx.semantic_defs[semantic_var_id].ty();
        let var = self.introduce_variable(ctx, ty);

        assert!(
            self.pulled_semantic_vars.insert(semantic_var_id, var.0).is_none(),
            "Semantic variable introduced more than once as input to the block"
        );

        self.semantic_variables.put(semantic_var_id, var)
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
    block: BlockScope,
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
        pulls: &[semantic::VarId],
        pushes: &[semantic::VarId],
    ) -> Block {
        let BlockSealed { mut block, end } = self;
        // Pull extra semantic variables if necessary.
        for semantic_var_id in pulls {
            if !block.pulled_semantic_vars.contains_key(semantic_var_id) {
                block.pull_semantic_variable(ctx, *semantic_var_id);
            }
        }
        // Make sure inputs contain all pulled semantic variables.
        for pulled_semantic_var_id in block.pulled_semantic_vars.keys() {
            assert!(
                pulls.contains(pulled_semantic_var_id),
                "finalize() called with missing input semantic variables."
            );
        }
        assert_eq!(block.pulled_semantic_vars.len(), pulls.len());

        // Get input variables in order.
        let inputs = pulls
            .iter()
            .map(|semantic_var_id| block.pulled_semantic_vars[*semantic_var_id])
            .collect();

        // Compute drops.
        let end = match end {
            BlockSealedEnd::Callsite(maybe_output) => {
                let pushes = pushes.iter().map(|semantic_var_id| {
                    // TODO(spapini): Convert to a diagnostic.
                    block
                        .semantic_variables
                        .take(*semantic_var_id)
                        .expect("finalize() called with dead output semantic variables.")
                        .var()
                        .expect("Value already moved.")
                        .0
                });
                let outputs = chain!(maybe_output.into_iter(), pushes).collect();
                BlockEnd::Callsite(outputs)
            }
            BlockSealedEnd::Return(returns) => BlockEnd::Return(returns),
            BlockSealedEnd::Unreachable => BlockEnd::Unreachable,
        };
        let drops = block.living_variables.into_iter().collect();

        Block { inputs, statements: block.statements, drops, end }
    }
}
