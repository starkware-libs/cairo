use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs as defs;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId, ModuleItemId, NamedLanguageElementLongId};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::{self as semantic, ConcreteTypeId, TypeId, TypeLongId, corelib};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{Intern, LookupIntern, Upcast};
use defs::ids::NamedLanguageElementId;
use itertools::Itertools;
use num_traits::ToPrimitive;

use crate::add_withdraw_gas::add_withdraw_gas;
use crate::borrow_check::{PotentialDestructCalls, borrow_check};
use crate::cache::load_cached_crate_functions;
use crate::concretize::concretize_lowered;
use crate::destructs::add_destructs;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind};
use crate::graph_algorithms::feedback_set::flag_add_withdraw_gas;
use crate::ids::{FunctionId, FunctionLongId};
use crate::inline::get_inline_diagnostics;
use crate::lower::{MultiLowering, lower_semantic_function};
use crate::optimizations::config::OptimizationConfig;
use crate::optimizations::scrub_units::scrub_units;
use crate::optimizations::strategy::{OptimizationStrategy, OptimizationStrategyId};
use crate::panic::lower_panics;
use crate::utils::InliningStrategy;
use crate::{
    BlockId, DependencyType, FlatBlockEnd, FlatLowered, Location, MatchInfo, Statement, ids,
};

// Salsa database interface.
#[salsa::query_group(LoweringDatabase)]
pub trait LoweringGroup: SemanticGroup + Upcast<dyn SemanticGroup> {
    #[salsa::interned]
    fn intern_lowering_function(&self, id: ids::FunctionLongId) -> ids::FunctionId;
    #[salsa::interned]
    fn intern_lowering_concrete_function_with_body(
        &self,
        id: ids::ConcreteFunctionWithBodyLongId,
    ) -> ids::ConcreteFunctionWithBodyId;
    #[salsa::interned]
    fn intern_lowering_function_with_body(
        &self,
        id: ids::FunctionWithBodyLongId,
    ) -> ids::FunctionWithBodyId;

    #[salsa::interned]
    fn intern_location(&self, id: Location) -> ids::LocationId;

    #[salsa::interned]
    fn intern_strategy(&self, id: OptimizationStrategy) -> OptimizationStrategyId;

    /// Computes the lowered representation of a function with a body, along with all it generated
    /// functions (e.g. closures, lambdas, loops, ...).
    fn priv_function_with_body_multi_lowering(
        &self,
        function_id: defs::ids::FunctionWithBodyId,
    ) -> Maybe<Arc<MultiLowering>>;

    /// Returns a mapping from function ids to their multi-lowerings for the given loaded from a
    /// cache for the given crate.
    fn cached_multi_lowerings(
        &self,
        crate_id: cairo_lang_filesystem::ids::CrateId,
    ) -> Option<Arc<OrderedHashMap<defs::ids::FunctionWithBodyId, MultiLowering>>>;

    /// Computes the lowered representation of a function with a body before borrow checking.
    fn priv_function_with_body_lowering(
        &self,
        function_id: ids::FunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Computes the lowered representation of a function with a body.
    /// Additionally applies borrow checking testing, and returns the possible calls per block.
    fn function_with_body_lowering_with_borrow_check(
        &self,
        function_id: ids::FunctionWithBodyId,
    ) -> Maybe<(Arc<FlatLowered>, Arc<PotentialDestructCalls>)>;

    /// Computes the lowered representation of a function with a body.
    fn function_with_body_lowering(
        &self,
        function_id: ids::FunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// A concrete version of priv_function_with_body_multi_lowering
    fn priv_concrete_function_with_body_lowered_flat(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Computes the lowered representation after the panic phase.
    fn concrete_function_with_body_postpanic_lowered(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Applies optimizations to the post_panic lowering.
    fn optimized_concrete_function_with_body_lowered(
        &self,
        function: ids::ConcreteFunctionWithBodyId,
        optimization_strategy: OptimizationStrategyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Computes the lowered representation of a function to be considered for inlining.
    fn inlined_function_with_body_lowered(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Computes the final lowered representation (after all the internal transformations).
    fn final_concrete_function_with_body_lowered(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Returns the set of direct callees of a concrete function with a body after the inline phase.
    fn concrete_function_with_body_direct_callees(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Maybe<Vec<ids::FunctionId>>;

    /// Returns the set of direct callees of a concrete function after the baseline optimization
    /// phase.
    fn concrete_function_with_body_inlined_direct_callees(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Maybe<Vec<ids::FunctionId>>;

    /// Returns the set of direct callees which are functions with body of a concrete function with
    /// a body (i.e. excluding libfunc callees), after the inline phase.
    fn concrete_function_with_body_direct_callees_with_body(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>>;

    /// Returns the set of direct callees which are functions with body of a concrete function with
    /// a body (i.e. excluding libfunc callees), after the baseline optimization phase.
    fn concrete_function_with_body_inlined_direct_callees_with_body(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>>;

    /// Returns the set of direct callees which are functions with body of a concrete function with
    /// a body (i.e. excluding libfunc callees), after all optimization phases.
    fn final_concrete_function_with_body_lowered_direct_callees(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>>;

    /// Aggregates function level lowering diagnostics.
    fn function_with_body_lowering_diagnostics(
        &self,
        function_id: ids::FunctionWithBodyId,
    ) -> Maybe<Diagnostics<LoweringDiagnostic>>;
    /// Aggregates semantic function level lowering diagnostics - along with all its generated
    /// function.
    fn semantic_function_with_body_lowering_diagnostics(
        &self,
        function_id: defs::ids::FunctionWithBodyId,
    ) -> Maybe<Diagnostics<LoweringDiagnostic>>;
    /// Aggregates module level lowering diagnostics.
    fn module_lowering_diagnostics(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Diagnostics<LoweringDiagnostic>>;

    /// Aggregates file level lowering diagnostics.
    fn file_lowering_diagnostics(&self, file_id: FileId) -> Maybe<Diagnostics<LoweringDiagnostic>>;

    // ### Queries related to implicits ###

    /// Returns all the implicit parameters that the function requires (according to both its
    /// signature and the functions it calls). The items in the returned vector are unique and the
    /// order is consistent, but not necessarily related to the order of the explicit implicits in
    /// the signature of the function.
    #[salsa::invoke(crate::implicits::function_implicits)]
    fn function_implicits(&self, function: ids::FunctionId) -> Maybe<Vec<TypeId>>;

    /// Returns all the implicits used by a strongly connected component of functions.
    #[salsa::invoke(crate::implicits::scc_implicits)]
    fn scc_implicits(&self, function: ConcreteSCCRepresentative) -> Maybe<Vec<TypeId>>;

    // ### Queries related to panics ###

    /// Returns whether the function may panic.
    #[salsa::invoke(crate::panic::function_may_panic)]
    fn function_may_panic(&self, function: ids::FunctionId) -> Maybe<bool>;

    /// Returns whether any function in the strongly connected component may panic.
    #[salsa::invoke(crate::panic::scc_may_panic)]
    fn scc_may_panic(&self, scc: ConcreteSCCRepresentative) -> Maybe<bool>;

    /// Checks if the function has a block that ends with panic.
    #[salsa::invoke(crate::panic::has_direct_panic)]
    fn has_direct_panic(&self, function_id: ids::ConcreteFunctionWithBodyId) -> Maybe<bool>;

    // ### cycles ###

    /// Returns the set of direct callees of a function with a body.
    #[salsa::invoke(crate::graph_algorithms::cycles::function_with_body_direct_callees)]
    fn function_with_body_direct_callees(
        &self,
        function_id: ids::FunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Maybe<OrderedHashSet<ids::FunctionId>>;
    /// Returns the set of direct callees which are functions with body of a function with a body
    /// (i.e. excluding libfunc callees).
    #[salsa::invoke(
        crate::graph_algorithms::cycles::function_with_body_direct_function_with_body_callees
    )]
    fn function_with_body_direct_function_with_body_callees(
        &self,
        function_id: ids::FunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Maybe<OrderedHashSet<ids::FunctionWithBodyId>>;

    /// Returns `true` if the function (in its final lowering representation) calls (possibly
    /// indirectly) itself, or if it calls (possibly indirectly) such a function. For example, if f0
    /// calls f1, f1 calls f2, f2 calls f3, and f3 calls f2, then [Self::final_contains_call_cycle]
    /// will return `true` for all of these functions.
    #[salsa::invoke(crate::graph_algorithms::cycles::final_contains_call_cycle)]
    #[salsa::cycle(crate::graph_algorithms::cycles::final_contains_call_cycle_handle_cycle)]
    fn final_contains_call_cycle(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<bool>;

    /// Returns `true` if the function calls (possibly indirectly) itself. For example, if f0 calls
    /// f1, f1 calls f2, f2 calls f3, and f3 calls f2, then [Self::in_cycle] will return
    /// `true` for f2 and f3, but false for f0 and f1.
    #[salsa::invoke(crate::graph_algorithms::cycles::in_cycle)]
    fn in_cycle(
        &self,
        function_id: ids::FunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Maybe<bool>;

    // ### Strongly connected components ###

    /// Returns the representative of the concrete function's strongly connected component. The
    /// representative is consistently chosen for all the concrete functions in the same SCC.
    #[salsa::invoke(
        crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_scc_representative
    )]
    fn concrete_function_with_body_scc_representative(
        &self,
        function: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> ConcreteSCCRepresentative;

    /// Returns all the concrete functions in the same strongly connected component as the given
    /// concrete function.
    #[salsa::invoke(
        crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_scc
    )]
    fn concrete_function_with_body_scc(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Vec<ids::ConcreteFunctionWithBodyId>;

    /// Returns the representative of the concrete function's strongly connected component. The
    /// representative is consistently chosen for all the concrete functions in the same SCC.
    /// This is using the representation after the baseline optimization phase.
    #[salsa::invoke(
        crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_scc_inlined_representative
    )]
    fn concrete_function_with_body_scc_inlined_representative(
        &self,
        function: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> ConcreteSCCRepresentative;

    /// Returns all the concrete functions in the same strongly connected component as the given
    /// concrete function.
    /// This is using the representation after the baseline optimization phase.
    #[salsa::invoke(
        crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_inlined_scc
    )]
    fn concrete_function_with_body_inlined_scc(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Vec<ids::ConcreteFunctionWithBodyId>;

    /// Returns all the functions in the same strongly connected component as the given function.
    #[salsa::invoke(crate::scc::function_with_body_scc)]
    fn function_with_body_scc(
        &self,
        function_id: ids::FunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Vec<ids::FunctionWithBodyId>;

    // ### Feedback set ###

    /// Returns the feedback-vertex-set of the given concrete function. A feedback-vertex-set is the
    /// set of vertices whose removal leaves a graph without cycles.
    #[salsa::invoke(crate::graph_algorithms::feedback_set::function_with_body_feedback_set)]
    fn function_with_body_feedback_set(
        &self,
        function: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<OrderedHashSet<ids::ConcreteFunctionWithBodyId>>;

    /// Returns whether the given function needs an additional withdraw_gas call.
    #[salsa::invoke(crate::graph_algorithms::feedback_set::needs_withdraw_gas)]
    fn needs_withdraw_gas(&self, function: ids::ConcreteFunctionWithBodyId) -> Maybe<bool>;

    /// Returns the feedback-vertex-set of the given concrete-function SCC-representative. A
    /// feedback-vertex-set is the set of vertices whose removal leaves a graph without cycles.
    #[salsa::invoke(crate::graph_algorithms::feedback_set::priv_function_with_body_feedback_set_of_representative)]
    fn priv_function_with_body_feedback_set_of_representative(
        &self,
        function: ConcreteSCCRepresentative,
    ) -> Maybe<OrderedHashSet<ids::ConcreteFunctionWithBodyId>>;

    /// Internal query for reorder_statements to cache the function ids that can be moved.
    #[salsa::invoke(crate::optimizations::config::priv_movable_function_ids)]
    fn priv_movable_function_ids(&self) -> Arc<UnorderedHashSet<ids::FunctionId>>;

    /// Internal query for the libfuncs information required for const folding.
    #[salsa::invoke(crate::optimizations::const_folding::priv_const_folding_info)]
    fn priv_const_folding_info(
        &self,
    ) -> Arc<crate::optimizations::const_folding::ConstFoldingLibfuncInfo>;

    // Internal query for a heuristic to decide if a given `function_id` should be inlined.
    #[salsa::invoke(crate::inline::priv_should_inline)]
    fn priv_should_inline(&self, function_id: ids::ConcreteFunctionWithBodyId) -> Maybe<bool>;

    /// Returns the configuration struct that controls the behavior of the optimization passes.
    #[salsa::input]
    fn optimization_config(&self) -> Arc<OptimizationConfig>;

    /// Returns the final optimization strategy that is applied on top of
    /// inlined_function_optimization_strategy.
    #[salsa::invoke(crate::optimizations::strategy::final_optimization_strategy)]
    fn final_optimization_strategy(&self) -> OptimizationStrategyId;

    /// Returns the baseline optimization strategy.
    /// This strategy is used for inlining decistion and as a starting point for the final lowering.
    #[salsa::invoke(crate::optimizations::strategy::baseline_optimization_strategy)]
    fn baseline_optimization_strategy(&self) -> OptimizationStrategyId;

    /// Returns the expected size of a type.
    fn type_size(&self, ty: TypeId) -> usize;
}

pub fn init_lowering_group(
    db: &mut (dyn LoweringGroup + 'static),
    inlining_strategy: InliningStrategy,
) {
    let mut moveable_functions: Vec<String> =
        ["bool_not_impl", "felt252_add", "felt252_sub", "felt252_mul", "felt252_div"]
            .into_iter()
            .map(|s| s.to_string())
            .collect();

    for ty in ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "u128"] {
        moveable_functions.push(format!("integer::{}_wide_mul", ty));
    }

    db.set_optimization_config(Arc::new(
        OptimizationConfig::default()
            .with_moveable_functions(moveable_functions)
            .with_inlining_strategy(inlining_strategy),
    ));
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct GenericSCCRepresentative(pub ids::FunctionWithBodyId);

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct ConcreteSCCRepresentative(pub ids::ConcreteFunctionWithBodyId);

// *** Main lowering phases in order.

fn priv_function_with_body_multi_lowering(
    db: &dyn LoweringGroup,
    function_id: defs::ids::FunctionWithBodyId,
) -> Maybe<Arc<MultiLowering>> {
    let crate_id = function_id.module_file_id(db.upcast()).0.owning_crate(db.upcast());
    if let Some(map) = db.cached_multi_lowerings(crate_id) {
        if let Some(multi_lowering) = map.get(&function_id) {
            return Ok(Arc::new(multi_lowering.clone()));
        } else {
            panic!("function not found in cached lowering {:?}", function_id.debug(db));
        }
    };

    let multi_lowering = lower_semantic_function(db.upcast(), function_id)?;
    Ok(Arc::new(multi_lowering))
}

fn cached_multi_lowerings(
    db: &dyn LoweringGroup,
    crate_id: cairo_lang_filesystem::ids::CrateId,
) -> Option<Arc<OrderedHashMap<defs::ids::FunctionWithBodyId, MultiLowering>>> {
    load_cached_crate_functions(db, crate_id)
}

// * Borrow checking.
fn priv_function_with_body_lowering(
    db: &dyn LoweringGroup,
    function_id: ids::FunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let semantic_function_id = function_id.base_semantic_function(db);
    let multi_lowering = db.priv_function_with_body_multi_lowering(semantic_function_id)?;
    let lowered = match &function_id.lookup_intern(db) {
        ids::FunctionWithBodyLongId::Semantic(_) => multi_lowering.main_lowering.clone(),
        ids::FunctionWithBodyLongId::Generated { key, .. } => {
            multi_lowering.generated_lowerings[key].clone()
        }
    };
    Ok(Arc::new(lowered))
}

fn function_with_body_lowering_with_borrow_check(
    db: &dyn LoweringGroup,
    function_id: ids::FunctionWithBodyId,
) -> Maybe<(Arc<FlatLowered>, Arc<PotentialDestructCalls>)> {
    let mut lowered = (*db.priv_function_with_body_lowering(function_id)?).clone();
    let block_extra_calls =
        borrow_check(db, function_id.to_concrete(db)?.is_panic_destruct_fn(db)?, &mut lowered);
    Ok((Arc::new(lowered), Arc::new(block_extra_calls)))
}

fn function_with_body_lowering(
    db: &dyn LoweringGroup,
    function_id: ids::FunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    Ok(db.function_with_body_lowering_with_borrow_check(function_id)?.0)
}

// * Concretizes lowered representation (monomorphization).
fn priv_concrete_function_with_body_lowered_flat(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let semantic_db = db.upcast();
    let mut lowered =
        (*db.function_with_body_lowering(function.function_with_body_id(db))?).clone();
    concretize_lowered(db, &mut lowered, &function.substitution(semantic_db)?)?;
    Ok(Arc::new(lowered))
}

// * Adds `withdraw_gas` calls.
// * Adds panics.
// * Adds destructor calls.
fn concrete_function_with_body_postpanic_lowered(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let mut lowered = (*db.priv_concrete_function_with_body_lowered_flat(function)?).clone();

    add_withdraw_gas(db, function, &mut lowered)?;
    lowered = lower_panics(db, function, &lowered)?;
    add_destructs(db, function, &mut lowered);
    scrub_units(db, &mut lowered);

    Ok(Arc::new(lowered))
}

/// Query implementation of [LoweringGroup::optimized_concrete_function_with_body_lowered].
fn optimized_concrete_function_with_body_lowered(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
    optimization_strategy: OptimizationStrategyId,
) -> Maybe<Arc<FlatLowered>> {
    let mut lowered = (*db.concrete_function_with_body_postpanic_lowered(function)?).clone();
    optimization_strategy.apply_strategy(db, function, &mut lowered)?;
    Ok(Arc::new(lowered))
}

/// Query implementation of [LoweringGroup::inlined_function_with_body_lowered].
fn inlined_function_with_body_lowered(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    db.optimized_concrete_function_with_body_lowered(function, db.baseline_optimization_strategy())
}

/// Query implementation of [LoweringGroup::final_concrete_function_with_body_lowered].
fn final_concrete_function_with_body_lowered(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    // Start from the `inlined_function_with_body_lowered` as it might already be computed.
    let mut lowered = (*db.inlined_function_with_body_lowered(function)?).clone();

    db.final_optimization_strategy().apply_strategy(db, function, &mut lowered)?;
    Ok(Arc::new(lowered))
}

/// Given the lowering of a function, returns the set of direct dependencies of that function,
/// according to the given [DependencyType]. See [DependencyType] for more information about
/// what is considered a dependency.
pub(crate) fn get_direct_callees(
    db: &dyn LoweringGroup,
    lowered_function: &FlatLowered,
    dependency_type: DependencyType,
    block_extra_calls: &UnorderedHashMap<BlockId, Vec<FunctionId>>,
) -> Vec<ids::FunctionId> {
    let mut direct_callees = Vec::new();
    if lowered_function.blocks.is_empty() {
        return direct_callees;
    }
    let withdraw_gas_fns = corelib::core_withdraw_gas_fns(db.upcast())
        .map(|id| FunctionLongId::Semantic(id).intern(db));
    let mut visited = vec![false; lowered_function.blocks.len()];
    let mut stack = vec![BlockId(0)];
    while let Some(block_id) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;
        let block = &lowered_function.blocks[block_id];
        for statement in &block.statements {
            if let Statement::Call(statement_call) = statement {
                // If the dependency_type is DependencyType::Cost and this call has a coupon input,
                // then the call statement has a constant cost and therefore there
                // is no cost dependency in the called function.
                if dependency_type != DependencyType::Cost || !statement_call.with_coupon {
                    direct_callees.push(statement_call.function);
                }
            }
        }
        if let Some(extra_calls) = block_extra_calls.get(&block_id) {
            direct_callees.extend(extra_calls.iter().copied());
        }
        match &block.end {
            FlatBlockEnd::NotSet | FlatBlockEnd::Return(..) | FlatBlockEnd::Panic(_) => {}
            FlatBlockEnd::Goto(next, _) => stack.push(*next),
            FlatBlockEnd::Match { info } => {
                let mut arms = info.arms().iter();
                if let MatchInfo::Extern(s) = info {
                    direct_callees.push(s.function);
                    if DependencyType::Cost == dependency_type
                        && withdraw_gas_fns.contains(&s.function)
                    {
                        // Not following the option when successfully fetched gas.
                        arms.next();
                    }
                }
                stack.extend(arms.map(|arm| arm.block_id));
            }
        }
    }
    direct_callees
}

fn concrete_function_with_body_direct_callees(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<Vec<ids::FunctionId>> {
    let lowered_function = db.priv_concrete_function_with_body_lowered_flat(function_id)?;
    Ok(get_direct_callees(db, &lowered_function, dependency_type, &Default::default()))
}

fn concrete_function_with_body_inlined_direct_callees(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<Vec<ids::FunctionId>> {
    let lowered_function = db.inlined_function_with_body_lowered(function_id)?;
    Ok(get_direct_callees(db, &lowered_function, dependency_type, &Default::default()))
}

/// Given a vector of FunctionIds returns the vector of FunctionWithBodyIds of the
/// [ids::ConcreteFunctionWithBodyId]s.
///
/// If `dependency_type` is `DependencyType::Cost`, returns the coupon functions when
/// `coupon_buy` and `coupon_refund` are encountered.
/// For example, for `coupon_buy::<foo::Coupon>()`, `foo` will be added to the list.
fn functions_with_body_from_function_ids(
    db: &dyn LoweringGroup,
    function_ids: Vec<ids::FunctionId>,
    dependency_type: DependencyType,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>> {
    Ok(function_ids
        .into_iter()
        .map(|concrete| {
            if dependency_type == DependencyType::Cost {
                if let Some(function_with_body) = extract_coupon_function(db, concrete)? {
                    return Ok(Some(function_with_body));
                }
            }
            concrete.body(db.upcast())
        })
        .collect::<Maybe<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect_vec())
}

/// Given a [ids::FunctionId] that represents `coupon_buy` or `coupon_refund`, returns the coupon's
/// function.
///
/// For example, `coupon_buy::<foo::Coupon>` will return `foo`.
fn extract_coupon_function(
    db: &dyn LoweringGroup,
    concrete: ids::FunctionId,
) -> Maybe<Option<ids::ConcreteFunctionWithBodyId>> {
    // Check that the function is a semantic function.
    let ids::FunctionLongId::Semantic(function_id) = concrete.lookup_intern(db) else {
        return Ok(None);
    };

    // Check that it's an extern function named "coupon_buy" or "coupon_refund".
    let concrete_function = function_id.get_concrete(db.upcast());
    let generic_function = concrete_function.generic_function;
    let semantic::items::functions::GenericFunctionId::Extern(extern_function_id) =
        generic_function
    else {
        return Ok(None);
    };
    let name = extern_function_id.lookup_intern(db).name(db.upcast());
    if !(name == "coupon_buy" || name == "coupon_refund") {
        return Ok(None);
    }

    // Extract the coupon function from the generic argument.
    let [semantic::GenericArgumentId::Type(type_id)] = concrete_function.generic_args[..] else {
        panic!("Unexpected generic_args for coupon_buy().");
    };
    let semantic::TypeLongId::Coupon(coupon_function) = type_id.lookup_intern(db) else {
        panic!("Unexpected generic_args for coupon_buy().");
    };

    // Convert [semantic::FunctionId] to [ids::ConcreteFunctionWithBodyId].
    let Some(coupon_function_with_body_id) =
        coupon_function.get_concrete(db.upcast()).body(db.upcast())?
    else {
        panic!("Unexpected generic_args for coupon_buy().");
    };

    Ok(Some(ids::ConcreteFunctionWithBodyId::from_semantic(db, coupon_function_with_body_id)))
}

fn concrete_function_with_body_direct_callees_with_body(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>> {
    functions_with_body_from_function_ids(
        db,
        db.concrete_function_with_body_direct_callees(function_id, dependency_type)?,
        dependency_type,
    )
}

fn concrete_function_with_body_inlined_direct_callees_with_body(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>> {
    functions_with_body_from_function_ids(
        db,
        db.concrete_function_with_body_inlined_direct_callees(function_id, dependency_type)?,
        dependency_type,
    )
}

fn final_concrete_function_with_body_lowered_direct_callees(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>> {
    let lowered_function = db.final_concrete_function_with_body_lowered(function_id)?;
    functions_with_body_from_function_ids(
        db,
        get_direct_callees(db, &lowered_function, dependency_type, &Default::default()),
        dependency_type,
    )
}

fn function_with_body_lowering_diagnostics(
    db: &dyn LoweringGroup,
    function_id: ids::FunctionWithBodyId,
) -> Maybe<Diagnostics<LoweringDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();

    if let Ok(lowered) = db.function_with_body_lowering(function_id) {
        diagnostics.extend(lowered.diagnostics.clone());
        if flag_add_withdraw_gas(db)
            && !lowered.signature.panicable
            && db.in_cycle(function_id, DependencyType::Cost)?
        {
            let location =
                Location::new(function_id.base_semantic_function(db).stable_location(db.upcast()));
            diagnostics.add(LoweringDiagnostic {
                location,
                kind: LoweringDiagnosticKind::NoPanicFunctionCycle,
            });
        }
    }

    if let Ok(diag) = get_inline_diagnostics(db, function_id) {
        diagnostics.extend(diag);
    }

    Ok(diagnostics.build())
}

fn semantic_function_with_body_lowering_diagnostics(
    db: &dyn LoweringGroup,
    semantic_function_id: defs::ids::FunctionWithBodyId,
) -> Maybe<Diagnostics<LoweringDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();

    if let Ok(multi_lowering) = db.priv_function_with_body_multi_lowering(semantic_function_id) {
        let function_id = ids::FunctionWithBodyLongId::Semantic(semantic_function_id).intern(db);
        diagnostics
            .extend(db.function_with_body_lowering_diagnostics(function_id).unwrap_or_default());
        for (key, _) in multi_lowering.generated_lowerings.iter() {
            let function_id =
                ids::FunctionWithBodyLongId::Generated { parent: semantic_function_id, key: *key }
                    .intern(db);
            diagnostics.extend(
                db.function_with_body_lowering_diagnostics(function_id).unwrap_or_default(),
            );
        }
    }

    Ok(diagnostics.build())
}

fn module_lowering_diagnostics(
    db: &dyn LoweringGroup,
    module_id: ModuleId,
) -> Maybe<Diagnostics<LoweringDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for item in db.module_items(module_id)?.iter() {
        match item {
            ModuleItemId::FreeFunction(free_function) => {
                let function_id = defs::ids::FunctionWithBodyId::Free(*free_function);
                diagnostics
                    .extend(db.semantic_function_with_body_lowering_diagnostics(function_id)?);
            }
            ModuleItemId::Constant(_) => {}
            ModuleItemId::Submodule(_) => {}
            ModuleItemId::Use(_) => {}
            ModuleItemId::Struct(_) => {}
            ModuleItemId::Enum(_) => {}
            ModuleItemId::TypeAlias(_) => {}
            ModuleItemId::ImplAlias(_) => {}
            ModuleItemId::Trait(trait_id) => {
                for trait_func in db.trait_functions(*trait_id)?.values() {
                    if matches!(db.trait_function_body(*trait_func), Ok(Some(_))) {
                        let function_id = defs::ids::FunctionWithBodyId::Trait(*trait_func);
                        diagnostics.extend(
                            db.semantic_function_with_body_lowering_diagnostics(function_id)?,
                        );
                    }
                }
            }
            ModuleItemId::Impl(impl_def_id) => {
                for impl_func in db.impl_functions(*impl_def_id)?.values() {
                    let function_id = defs::ids::FunctionWithBodyId::Impl(*impl_func);
                    diagnostics
                        .extend(db.semantic_function_with_body_lowering_diagnostics(function_id)?);
                }
            }
            ModuleItemId::ExternType(_) => {}
            ModuleItemId::ExternFunction(_) => {}
        }
    }
    Ok(diagnostics.build())
}

fn file_lowering_diagnostics(
    db: &dyn LoweringGroup,
    file_id: FileId,
) -> Maybe<Diagnostics<LoweringDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for module_id in db.file_modules(file_id)?.iter().copied() {
        if let Ok(module_diagnostics) = db.module_lowering_diagnostics(module_id) {
            diagnostics.extend(module_diagnostics)
        }
    }
    Ok(diagnostics.build())
}

fn type_size(db: &dyn LoweringGroup, ty: TypeId) -> usize {
    match ty.lookup_intern(db) {
        TypeLongId::Concrete(concrete_type_id) => match concrete_type_id {
            ConcreteTypeId::Struct(struct_id) => db
                .concrete_struct_members(struct_id)
                .unwrap()
                .iter()
                .map(|(_, member)| db.type_size(member.ty))
                .sum::<usize>(),
            ConcreteTypeId::Enum(enum_id) => {
                1 + db
                    .concrete_enum_variants(enum_id)
                    .unwrap()
                    .into_iter()
                    .map(|variant| db.type_size(variant.ty))
                    .max()
                    .unwrap_or_default()
            }
            ConcreteTypeId::Extern(extern_id) => {
                match extern_id.extern_type_id(db.upcast()).name(db.upcast()).as_str() {
                    "Array" | "SquashedFelt252Dict" | "EcPoint" => 2,
                    "EcState" => 3,
                    "Uint128MulGuarantee" => 4,
                    _ => 1,
                }
            }
        },
        TypeLongId::Tuple(types) => types.into_iter().map(|ty| db.type_size(ty)).sum::<usize>(),
        TypeLongId::Snapshot(ty) => db.type_size(ty),
        TypeLongId::FixedSizeArray { type_id, size } => {
            db.type_size(type_id)
                * size
                    .lookup_intern(db)
                    .into_int()
                    .expect("Expected ConstValue::Int for size")
                    .to_usize()
                    .unwrap()
        }
        TypeLongId::Closure(closure_ty) => {
            closure_ty.captured_types.iter().map(|ty| db.type_size(*ty)).sum()
        }
        TypeLongId::Coupon(_) => 0,
        TypeLongId::GenericParameter(_)
        | TypeLongId::Var(_)
        | TypeLongId::ImplType(_)
        | TypeLongId::Missing(_) => {
            panic!("Function should only be called with fully concrete types")
        }
    }
}
