use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs as defs;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    ExternFunctionId, LanguageElementId, ModuleId, ModuleItemId, NamedLanguageElementLongId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe, MaybeAsRef};
use cairo_lang_filesystem::ids::{FileId, Tracked};
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::imp::ImplSemantic;
use cairo_lang_semantic::items::macro_call::MacroCallSemantic;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_semantic::items::trt::TraitSemantic;
use cairo_lang_semantic::{self as semantic, ConcreteTypeId, TypeId, TypeLongId, corelib};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use defs::ids::NamedLanguageElementId;
use itertools::Itertools;
use num_traits::ToPrimitive;
use salsa::{Database, Setter};

use crate::add_withdraw_gas::add_withdraw_gas;
use crate::borrow_check::{BorrowCheckResult, borrow_check, borrow_check_possible_withdraw_gas};
use crate::cache::load_cached_crate_functions;
use crate::concretize::concretize_lowered;
use crate::destructs::add_destructs;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind};
use crate::graph_algorithms::feedback_set::flag_add_withdraw_gas;
use crate::ids::{ConcreteFunctionWithBodyId, FunctionId, FunctionLongId, GenericOrSpecialized};
use crate::inline::get_inline_diagnostics;
use crate::inline::statements_weights::{ApproxCasmInlineWeight, InlineWeight};
use crate::lower::{MultiLowering, lower_semantic_function};
use crate::optimizations::config::Optimizations;
use crate::optimizations::scrub_units::scrub_units;
use crate::optimizations::strategy::OptimizationStrategyId;
use crate::panic::lower_panics;
use crate::specialization::specialized_function_lowered;
use crate::{
    BlockEnd, BlockId, DependencyType, Location, Lowered, LoweringStage, MatchInfo, Statement, ids,
};

type CodeSizeEstimator = fn(&dyn Database, ConcreteFunctionWithBodyId<'_>) -> Maybe<isize>;
#[salsa::input]
pub struct LoweringGroupInput {
    #[returns(ref)]
    pub optimizations: Option<Optimizations>,
    /// A configurable function to get estimated size of the function with the given id.
    #[returns(ref)]
    code_size_estimator: Option<CodeSizeEstimator>,
}

#[salsa::tracked(returns(ref))]
pub fn lowering_group_input(db: &dyn Database) -> LoweringGroupInput {
    LoweringGroupInput::new(db, None, None)
}

/// Trait for information over the lowering.
pub trait LoweringGroup: Database {
    /// Computes the lowered representation of a function with a body, along with all it generated
    /// functions (e.g. closures, lambdas, loops, ...).
    fn priv_function_with_body_multi_lowering<'db>(
        &'db self,
        function_id: defs::ids::FunctionWithBodyId<'db>,
    ) -> Maybe<&'db MultiLowering<'db>> {
        priv_function_with_body_multi_lowering(self.as_dyn_database(), (), function_id)
            .maybe_as_ref()
    }

    /// Returns a mapping from function ids to their multi-lowerings for the given loaded from a
    /// cache for the given crate.
    fn cached_multi_lowerings<'db>(
        &'db self,
        crate_id: cairo_lang_filesystem::ids::CrateId<'db>,
    ) -> Option<&'db OrderedHashMap<defs::ids::FunctionWithBodyId<'db>, MultiLowering<'db>>> {
        cached_multi_lowerings(self.as_dyn_database(), crate_id).as_ref()
    }

    /// Computes the lowered representation of a function with a body before borrow checking.
    fn function_with_body_lowering<'db>(
        &'db self,
        function_id: ids::FunctionWithBodyId<'db>,
    ) -> Maybe<&'db Lowered<'db>> {
        function_with_body_lowering(self.as_dyn_database(), function_id)
    }

    /// Computes the borrow check result of a function.
    fn borrow_check<'db>(
        &'db self,
        function_id: ids::FunctionWithBodyId<'db>,
    ) -> Maybe<&'db BorrowCheckResult<'db>> {
        borrow_check_tracked(self.as_dyn_database(), function_id).maybe_as_ref()
    }

    /// Computes the lowered representation of a function at the requested lowering stage.
    fn lowered_body<'db>(
        &'db self,
        function_id: ids::ConcreteFunctionWithBodyId<'db>,
        stage: LoweringStage,
    ) -> Maybe<&'db Lowered<'db>> {
        lowered_body(self.as_dyn_database(), function_id, stage).maybe_as_ref()
    }

    /// Returns the set of direct callees which are functions with body of a concrete function with
    /// a body (i.e. excluding libfunc callees), at the given stage.
    fn lowered_direct_callees<'db>(
        &'db self,
        function_id: ids::ConcreteFunctionWithBodyId<'db>,
        dependency_type: DependencyType,
        stage: LoweringStage,
    ) -> Maybe<&'db [ids::FunctionId<'db>]> {
        Ok(lowered_direct_callees(self.as_dyn_database(), function_id, dependency_type, stage)
            .maybe_as_ref()?)
    }

    /// Returns the set of direct callees which are functions with body of a concrete function with
    /// a body (i.e. excluding libfunc callees), at the given stage.
    fn lowered_direct_callees_with_body<'db>(
        &'db self,
        function_id: ids::ConcreteFunctionWithBodyId<'db>,
        dependency_type: DependencyType,
        stage: LoweringStage,
    ) -> Maybe<&'db [ids::ConcreteFunctionWithBodyId<'db>]> {
        Ok(lowered_direct_callees_with_body(
            self.as_dyn_database(),
            function_id,
            dependency_type,
            stage,
        )
        .maybe_as_ref()?)
    }

    /// Aggregates function level lowering diagnostics.
    fn function_with_body_lowering_diagnostics<'db>(
        &'db self,
        function_id: ids::FunctionWithBodyId<'db>,
    ) -> Maybe<Diagnostics<'db, LoweringDiagnostic<'db>>> {
        function_with_body_lowering_diagnostics(self.as_dyn_database(), function_id)
    }
    /// Aggregates semantic function level lowering diagnostics - along with all its generated
    /// function.
    fn semantic_function_with_body_lowering_diagnostics<'db>(
        &'db self,
        function_id: defs::ids::FunctionWithBodyId<'db>,
    ) -> Maybe<Diagnostics<'db, LoweringDiagnostic<'db>>> {
        semantic_function_with_body_lowering_diagnostics(self.as_dyn_database(), (), function_id)
    }
    /// Aggregates module level lowering diagnostics.
    fn module_lowering_diagnostics<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Diagnostics<'db, LoweringDiagnostic<'db>>> {
        module_lowering_diagnostics(self.as_dyn_database(), (), module_id)
    }

    /// Aggregates file level lowering diagnostics.
    fn file_lowering_diagnostics<'db>(
        &'db self,
        file_id: FileId<'db>,
    ) -> Maybe<Diagnostics<'db, LoweringDiagnostic<'db>>> {
        file_lowering_diagnostics(self.as_dyn_database(), file_id)
    }

    // ### Queries related to implicits ###

    /// Returns all the implicit parameters that the function requires (according to both its
    /// signature and the functions it calls). The items in the returned vector are unique and the
    /// order is consistent, but not necessarily related to the order of the explicit implicits in
    /// the signature of the function.
    fn function_implicits<'db>(
        &'db self,
        function: ids::FunctionId<'db>,
    ) -> Maybe<Vec<TypeId<'db>>> {
        crate::implicits::function_implicits(self.as_dyn_database(), function)
    }

    // ### Queries related to panics ###

    /// Returns whether the function may panic.
    fn function_may_panic<'db>(&'db self, function: ids::FunctionId<'db>) -> Maybe<bool> {
        crate::panic::function_may_panic(self.as_dyn_database(), function)
    }

    /// Checks if the function has a block that ends with panic.
    fn has_direct_panic<'db>(
        &'db self,
        function_id: ids::ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<bool> {
        crate::panic::has_direct_panic(self.as_dyn_database(), function_id)
    }

    // ### cycles ###

    /// Returns the set of direct callees of a function with a body.
    fn function_with_body_direct_callees<'db>(
        &'db self,
        function_id: ids::FunctionWithBodyId<'db>,
        dependency_type: DependencyType,
    ) -> Maybe<&'db OrderedHashSet<ids::FunctionId<'db>>> {
        crate::graph_algorithms::cycles::function_with_body_direct_callees(
            self.as_dyn_database(),
            function_id,
            dependency_type,
        )
        .maybe_as_ref()
    }
    /// Returns the set of direct callees which are functions with body of a function with a body
    /// (i.e. excluding libfunc callees).
    fn function_with_body_direct_function_with_body_callees<'db>(
        &'db self,
        function_id: ids::FunctionWithBodyId<'db>,
        dependency_type: DependencyType,
    ) -> Maybe<&'db OrderedHashSet<ids::FunctionWithBodyId<'db>>> {
        crate::graph_algorithms::cycles::function_with_body_direct_function_with_body_callees(
            self.as_dyn_database(),
            function_id,
            dependency_type,
        )
        .maybe_as_ref()
    }

    /// Returns `true` if the function (in its final lowering representation) calls (possibly
    /// indirectly) itself, or if it calls (possibly indirectly) such a function. For example, if f0
    /// calls f1, f1 calls f2, f2 calls f3, and f3 calls f2, then [Self::final_contains_call_cycle]
    /// will return `true` for all of these functions.
    fn final_contains_call_cycle<'db>(
        &'db self,
        function_id: ids::ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<bool> {
        crate::graph_algorithms::cycles::final_contains_call_cycle(
            self.as_dyn_database(),
            function_id,
        )
    }

    /// Returns `true` if the function calls (possibly indirectly) itself. For example, if f0 calls
    /// f1, f1 calls f2, f2 calls f3, and f3 calls f2, then [Self::in_cycle] will return
    /// `true` for f2 and f3, but false for f0 and f1.
    fn in_cycle<'db>(
        &'db self,
        function_id: ids::FunctionWithBodyId<'db>,
        dependency_type: DependencyType,
    ) -> Maybe<bool> {
        crate::graph_algorithms::cycles::in_cycle(
            self.as_dyn_database(),
            function_id,
            dependency_type,
        )
    }

    /// A concrete version of `in_cycle`.
    fn concrete_in_cycle<'db>(
        &'db self,
        function_id: ids::ConcreteFunctionWithBodyId<'db>,
        dependency_type: DependencyType,
        stage: LoweringStage,
    ) -> Maybe<bool> {
        crate::graph_algorithms::cycles::concrete_in_cycle(
            self.as_dyn_database(),
            function_id,
            dependency_type,
            stage,
        )
    }

    // ### Strongly connected components ###

    /// Returns the representative of the concrete function's strongly connected component. The
    /// representative is consistently chosen for all the concrete functions in the same SCC.
    fn lowered_scc_representative<'db>(
        &'db self,
        function: ids::ConcreteFunctionWithBodyId<'db>,
        dependency_type: DependencyType,
        stage: LoweringStage,
    ) -> ConcreteSCCRepresentative<'db> {
        crate::graph_algorithms::strongly_connected_components::lowered_scc_representative(
            self.as_dyn_database(),
            function,
            dependency_type,
            stage,
        )
    }

    /// Returns all the concrete functions in the same strongly connected component as the given
    /// concrete function.
    fn lowered_scc<'db>(
        &'db self,
        function_id: ids::ConcreteFunctionWithBodyId<'db>,
        dependency_type: DependencyType,
        stage: LoweringStage,
    ) -> Vec<ids::ConcreteFunctionWithBodyId<'db>> {
        crate::graph_algorithms::strongly_connected_components::lowered_scc(
            self.as_dyn_database(),
            function_id,
            dependency_type,
            stage,
        )
    }

    /// Returns all the functions in the same strongly connected component as the given function.
    fn function_with_body_scc<'db>(
        &'db self,
        function_id: ids::FunctionWithBodyId<'db>,
        dependency_type: DependencyType,
    ) -> &'db [ids::FunctionWithBodyId<'db>] {
        crate::scc::function_with_body_scc(self.as_dyn_database(), function_id, dependency_type)
    }

    // ### Feedback set ###

    /// Returns the feedback-vertex-set of the given concrete function. A feedback-vertex-set is the
    /// set of vertices whose removal leaves a graph without cycles.
    fn function_with_body_feedback_set<'db>(
        &'db self,
        function: ids::ConcreteFunctionWithBodyId<'db>,
        stage: LoweringStage,
    ) -> Maybe<&'db OrderedHashSet<ids::ConcreteFunctionWithBodyId<'db>>> {
        crate::graph_algorithms::feedback_set::function_with_body_feedback_set(
            self.as_dyn_database(),
            function,
            stage,
        )
        .maybe_as_ref()
    }

    /// Returns whether the given function needs an additional withdraw_gas call.
    fn needs_withdraw_gas<'db>(
        &'db self,
        function: ids::ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<bool> {
        crate::graph_algorithms::feedback_set::needs_withdraw_gas(self.as_dyn_database(), function)
    }

    /// Internal query for reorder_statements to cache the function ids that can be moved.
    fn priv_movable_function_ids<'db>(&'db self) -> &'db UnorderedHashSet<ExternFunctionId<'db>> {
        crate::optimizations::config::priv_movable_function_ids(self.as_dyn_database())
    }

    // Internal query for a heuristic to decide if a given `function_id` should be inlined.
    fn priv_should_inline<'db>(
        &'db self,
        function_id: ids::ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<bool> {
        crate::inline::priv_should_inline(self.as_dyn_database(), function_id)
    }

    // Internal query for if a function is marked as `#[inline(never)]`.
    fn priv_never_inline<'db>(
        &'db self,
        function_id: ids::ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<bool> {
        crate::inline::priv_never_inline(self.as_dyn_database(), function_id)
    }

    /// Returns whether a function should be specialized.
    fn priv_should_specialize<'db>(
        &'db self,
        function_id: ids::ConcreteFunctionWithBodyId<'db>,
    ) -> Maybe<bool> {
        crate::specialization::priv_should_specialize(self.as_dyn_database(), function_id)
    }

    /// Returns the configuration struct that controls the behavior of the optimization passes.
    fn optimizations(&self) -> &Optimizations {
        let db = self.as_dyn_database();
        lowering_group_input(db).optimizations(db).as_ref().unwrap()
    }

    /// Returns the final optimization strategy that is applied on top of
    /// inlined_function_optimization_strategy.
    fn final_optimization_strategy<'db>(&'db self) -> OptimizationStrategyId<'db> {
        crate::optimizations::strategy::final_optimization_strategy(self.as_dyn_database())
    }

    /// Returns the baseline optimization strategy.
    /// This strategy is used for inlining decision and as a starting point for the final lowering.
    fn baseline_optimization_strategy<'db>(&'db self) -> OptimizationStrategyId<'db> {
        crate::optimizations::strategy::baseline_optimization_strategy(self.as_dyn_database())
    }

    /// Returns the expected size of a type.
    fn type_size<'db>(&'db self, ty: TypeId<'db>) -> usize {
        type_size(self.as_dyn_database(), ty)
    }

    /// Returns the estimated size of the function with the given id.
    fn estimate_size<'db>(&'db self, function_id: ConcreteFunctionWithBodyId<'db>) -> Maybe<isize> {
        estimate_size(self.as_dyn_database(), function_id)
    }
}
impl<T: Database + ?Sized> LoweringGroup for T {}

pub fn init_lowering_group(
    db: &mut (dyn Database + 'static),
    optimizations: Optimizations,
    code_size_estimator: Option<CodeSizeEstimator>,
) {
    lowering_group_input(db).set_optimizations(db).to(Some(optimizations));
    lowering_group_input(db).set_code_size_estimator(db).to(code_size_estimator);
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct GenericSCCRepresentative<'db>(pub ids::FunctionWithBodyId<'db>);

#[derive(Debug, Eq, PartialEq, Clone, Hash, salsa::Update)]
pub struct ConcreteSCCRepresentative<'db>(pub ids::ConcreteFunctionWithBodyId<'db>);

// *** Main lowering phases in order.

#[salsa::tracked(returns(ref))]
fn priv_function_with_body_multi_lowering<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    function_id: defs::ids::FunctionWithBodyId<'db>,
) -> Maybe<MultiLowering<'db>> {
    let crate_id = function_id.parent_module(db).owning_crate(db);
    if let Some(map) = db.cached_multi_lowerings(crate_id) {
        if let Some(multi_lowering) = map.get(&function_id) {
            return Ok(multi_lowering.clone());
        } else {
            panic!("function not found in cached lowering {:?}", function_id.debug(db));
        }
    };

    lower_semantic_function(db, function_id)
}

#[salsa::tracked(returns(ref))]
fn cached_multi_lowerings<'db>(
    db: &'db dyn Database,
    crate_id: cairo_lang_filesystem::ids::CrateId<'db>,
) -> Option<OrderedHashMap<defs::ids::FunctionWithBodyId<'db>, MultiLowering<'db>>> {
    load_cached_crate_functions(db, crate_id)
}

fn function_with_body_lowering<'db>(
    db: &'db dyn Database,
    function_id: ids::FunctionWithBodyId<'db>,
) -> Maybe<&'db Lowered<'db>> {
    let semantic_function_id = function_id.base_semantic_function(db);
    let multi_lowering = db.priv_function_with_body_multi_lowering(semantic_function_id)?;
    Ok(match &function_id.long(db) {
        ids::FunctionWithBodyLongId::Semantic(_) => &multi_lowering.main_lowering,
        ids::FunctionWithBodyLongId::Generated { key, .. } => {
            &multi_lowering.generated_lowerings[key]
        }
    })
}

#[salsa::tracked(returns(ref))]
fn borrow_check_tracked<'db>(
    db: &'db dyn Database,
    function_id: ids::FunctionWithBodyId<'db>,
) -> Maybe<BorrowCheckResult<'db>> {
    let lowered = db.function_with_body_lowering(function_id)?;
    Ok(borrow_check(db, function_id.to_concrete(db)?.is_panic_destruct_fn(db)?, lowered))
}

#[salsa::tracked(returns(ref))]
fn lowered_body<'db>(
    db: &'db dyn Database,
    function: ids::ConcreteFunctionWithBodyId<'db>,
    stage: LoweringStage,
) -> Maybe<Lowered<'db>> {
    Ok(match stage {
        LoweringStage::Monomorphized => match function.generic_or_specialized(db) {
            GenericOrSpecialized::Generic(generic_function_id) => {
                db.function_with_body_lowering_diagnostics(generic_function_id)?
                    .check_error_free()?;
                let mut lowered = db.function_with_body_lowering(generic_function_id)?.clone();
                concretize_lowered(db, &mut lowered, &function.substitution(db)?)?;
                lowered
            }
            GenericOrSpecialized::Specialized(specialized) => {
                specialized_function_lowered(db, specialized)?
            }
        },
        LoweringStage::PreOptimizations => {
            let mut lowered = db.lowered_body(function, LoweringStage::Monomorphized)?.clone();
            add_withdraw_gas(db, function, &mut lowered)?;
            lower_panics(db, function, &mut lowered)?;
            add_destructs(db, function, &mut lowered);
            scrub_units(db, &mut lowered);
            lowered
        }
        LoweringStage::PostBaseline => {
            let mut lowered = db.lowered_body(function, LoweringStage::PreOptimizations)?.clone();
            db.baseline_optimization_strategy().apply_strategy(db, function, &mut lowered)?;
            lowered
        }
        LoweringStage::Final => {
            let mut lowered = db.lowered_body(function, LoweringStage::PostBaseline)?.clone();
            db.final_optimization_strategy().apply_strategy(db, function, &mut lowered)?;
            lowered
        }
    })
}

/// Given the lowering of a function, returns the set of direct dependencies of that function,
/// according to the given [DependencyType]. See [DependencyType] for more information about
/// what is considered a dependency.
pub(crate) fn get_direct_callees<'db>(
    db: &dyn Database,
    lowered_function: &Lowered<'db>,
    dependency_type: DependencyType,
    block_extra_calls: &UnorderedHashMap<BlockId, Vec<FunctionId<'db>>>,
) -> Vec<ids::FunctionId<'db>> {
    let mut direct_callees = Vec::new();
    if lowered_function.blocks.is_empty() {
        return direct_callees;
    }
    let withdraw_gas_fns =
        corelib::core_withdraw_gas_fns(db).map(|id| FunctionLongId::Semantic(id).intern(db));
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
            BlockEnd::NotSet | BlockEnd::Return(..) | BlockEnd::Panic(_) => {}
            BlockEnd::Goto(next, _) => stack.push(*next),
            BlockEnd::Match { info } => {
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

/// Given a vector of FunctionIds returns the vector of FunctionWithBodyIds of the
/// [ids::ConcreteFunctionWithBodyId]s.
///
/// If `dependency_type` is `DependencyType::Cost`, returns the coupon functions when
/// `coupon_buy` and `coupon_refund` are encountered.
/// For example, for `coupon_buy::<foo::Coupon>()`, `foo` will be added to the list.
fn functions_with_body_from_function_ids<'db>(
    db: &'db dyn Database,
    function_ids: &'db [ids::FunctionId<'db>],
    dependency_type: DependencyType,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId<'db>>> {
    Ok(function_ids
        .iter()
        .map(|concrete| {
            if dependency_type == DependencyType::Cost
                && let Some(function_with_body) = extract_coupon_function(db, *concrete)?
            {
                return Ok(Some(function_with_body));
            }
            concrete.body(db)
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
fn extract_coupon_function<'db>(
    db: &'db dyn Database,
    concrete: ids::FunctionId<'db>,
) -> Maybe<Option<ids::ConcreteFunctionWithBodyId<'db>>> {
    // Check that the function is a semantic function.
    let ids::FunctionLongId::Semantic(function_id) = concrete.long(db) else {
        return Ok(None);
    };

    // Check that it's an extern function named "coupon_buy" or "coupon_refund".
    let concrete_function = function_id.get_concrete(db);
    let generic_function = concrete_function.generic_function;
    let semantic::items::functions::GenericFunctionId::Extern(extern_function_id) =
        generic_function
    else {
        return Ok(None);
    };
    let name = extern_function_id.long(db).name(db).long(db);
    if !(name == "coupon_buy" || name == "coupon_refund") {
        return Ok(None);
    }

    // Extract the coupon function from the generic argument.
    let [semantic::GenericArgumentId::Type(type_id)] = concrete_function.generic_args[..] else {
        panic!("Unexpected generic_args for coupon_buy().");
    };
    let semantic::TypeLongId::Coupon(coupon_function) = type_id.long(db) else {
        panic!("Unexpected generic_args for coupon_buy().");
    };

    // Convert [semantic::FunctionId] to [ids::ConcreteFunctionWithBodyId].
    let Some(coupon_function_with_body_id) = coupon_function.get_concrete(db).body(db)? else {
        panic!("Unexpected generic_args for coupon_buy().");
    };

    Ok(Some(ids::ConcreteFunctionWithBodyId::from_semantic(db, coupon_function_with_body_id)))
}

#[salsa::tracked(returns(ref))]
fn lowered_direct_callees<'db>(
    db: &'db dyn Database,
    function_id: ids::ConcreteFunctionWithBodyId<'db>,
    dependency_type: DependencyType,
    stage: LoweringStage,
) -> Maybe<Vec<ids::FunctionId<'db>>> {
    let lowered_function = db.lowered_body(function_id, stage)?;
    Ok(get_direct_callees(db, lowered_function, dependency_type, &Default::default()))
}

#[salsa::tracked(returns(ref))]
fn lowered_direct_callees_with_body<'db>(
    db: &'db dyn Database,
    function_id: ids::ConcreteFunctionWithBodyId<'db>,
    dependency_type: DependencyType,
    stage: LoweringStage,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId<'db>>> {
    functions_with_body_from_function_ids(
        db,
        db.lowered_direct_callees(function_id, dependency_type, stage)?,
        dependency_type,
    )
}

#[salsa::tracked]
fn function_with_body_lowering_diagnostics<'db>(
    db: &'db dyn Database,
    function_id: ids::FunctionWithBodyId<'db>,
) -> Maybe<Diagnostics<'db, LoweringDiagnostic<'db>>> {
    let mut diagnostics = DiagnosticsBuilder::default();

    if let Ok(lowered) = db.function_with_body_lowering(function_id) {
        diagnostics.extend(lowered.diagnostics.clone());
        if let Ok(bc) = db.borrow_check(function_id) {
            diagnostics.extend(bc.diagnostics.clone());
        }
        if flag_add_withdraw_gas(db) && db.in_cycle(function_id, DependencyType::Cost)? {
            let location =
                Location::new(function_id.base_semantic_function(db).stable_location(db));
            if !lowered.signature.panicable {
                diagnostics.add(LoweringDiagnostic {
                    location: location.clone(),
                    kind: LoweringDiagnosticKind::NoPanicFunctionCycle,
                });
            }
            borrow_check_possible_withdraw_gas(db, location.intern(db), lowered, &mut diagnostics)
        }
    }

    if let Ok(diag) = get_inline_diagnostics(db, function_id) {
        diagnostics.extend(diag);
    }

    Ok(diagnostics.build())
}

#[salsa::tracked]
fn semantic_function_with_body_lowering_diagnostics<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    semantic_function_id: defs::ids::FunctionWithBodyId<'db>,
) -> Maybe<Diagnostics<'db, LoweringDiagnostic<'db>>> {
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

#[salsa::tracked]
fn module_lowering_diagnostics<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> Maybe<Diagnostics<'db, LoweringDiagnostic<'db>>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for item in module_id.module_data(db)?.items(db).iter() {
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
            ModuleItemId::MacroDeclaration(_) => {}
        }
    }
    for macro_call in db.module_macro_calls_ids(module_id)?.iter() {
        if let Ok(macro_module_id) = db.macro_call_module_id(*macro_call)
            && let Ok(lowering_diags) = db.module_lowering_diagnostics(macro_module_id)
        {
            diagnostics.extend(lowering_diags);
        }
    }
    Ok(diagnostics.build())
}

#[salsa::tracked]
fn file_lowering_diagnostics<'db>(
    db: &'db dyn Database,
    file_id: FileId<'db>,
) -> Maybe<Diagnostics<'db, LoweringDiagnostic<'db>>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for module_id in db.file_modules(file_id)?.iter().copied() {
        if let Ok(module_diagnostics) = db.module_lowering_diagnostics(module_id) {
            diagnostics.extend(module_diagnostics)
        }
    }
    Ok(diagnostics.build())
}

#[salsa::tracked]
fn type_size<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> usize {
    match ty.long(db) {
        TypeLongId::Concrete(concrete_type_id) => match concrete_type_id {
            ConcreteTypeId::Struct(struct_id) => db
                .concrete_struct_members(*struct_id)
                .unwrap()
                .iter()
                .map(|(_, member)| db.type_size(member.ty))
                .sum::<usize>(),
            ConcreteTypeId::Enum(enum_id) => {
                1 + db
                    .concrete_enum_variants(*enum_id)
                    .unwrap()
                    .into_iter()
                    .map(|variant| db.type_size(variant.ty))
                    .max()
                    .unwrap_or_default()
            }
            ConcreteTypeId::Extern(extern_id) => {
                match extern_id.extern_type_id(db).name(db).long(db).as_str() {
                    "Array" | "SquashedFelt252Dict" | "EcPoint" => 2,
                    "EcState" => 3,
                    "Uint128MulGuarantee" => 4,
                    _ => 1,
                }
            }
        },
        TypeLongId::Tuple(types) => types.iter().map(|ty| db.type_size(*ty)).sum::<usize>(),
        TypeLongId::Snapshot(ty) => db.type_size(*ty),
        TypeLongId::FixedSizeArray { type_id, size } => {
            db.type_size(*type_id)
                * size
                    .long(db)
                    .to_int()
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

fn estimate_size<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
) -> Maybe<isize> {
    let code_size_estimator = lowering_group_input(db).code_size_estimator(db);
    if let Some(estimator) = code_size_estimator {
        estimator(db, function_id)
    } else {
        // Calling fallback approximated size heuristic.
        let lowered = db.lowered_body(function_id, LoweringStage::PostBaseline)?;
        Ok(ApproxCasmInlineWeight::new(db, lowered).lowered_weight(lowered))
    }
}
