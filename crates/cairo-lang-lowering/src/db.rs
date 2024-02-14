use std::sync::Arc;

use cairo_lang_defs as defs;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId, ModuleItemId};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::{self as semantic, TypeId};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::Upcast;
use itertools::Itertools;
use semantic::corelib;

use crate::add_withdraw_gas::add_withdraw_gas;
use crate::borrow_check::borrow_check;
use crate::concretize::concretize_lowered;
use crate::destructs::add_destructs;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind};
use crate::graph_algorithms::feedback_set::flag_add_withdraw_gas;
use crate::ids::FunctionLongId;
use crate::implicits::lower_implicits;
use crate::inline::{apply_inlining, PrivInlineData};
use crate::lower::{lower_semantic_function, MultiLowering};
use crate::optimizations::branch_inversion::branch_inversion;
use crate::optimizations::cancel_ops::cancel_ops;
use crate::optimizations::config::OptimizationConfig;
use crate::optimizations::const_folding::const_folding;
use crate::optimizations::match_optimizer::optimize_matches;
use crate::optimizations::remappings::optimize_remappings;
use crate::optimizations::reorder_statements::reorder_statements;
use crate::optimizations::return_optimization::return_optimization;
use crate::panic::lower_panics;
use crate::reorganize_blocks::reorganize_blocks;
use crate::{
    ids, BlockId, DependencyType, FlatBlockEnd, FlatLowered, Location, MatchInfo, Statement,
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

    // Reports inlining diagnostics.
    #[salsa::invoke(crate::inline::priv_inline_data)]
    fn priv_inline_data(&self, function_id: ids::FunctionWithBodyId) -> Maybe<Arc<PrivInlineData>>;

    /// Computes the lowered representation of a function with a body, along with all it generated
    /// functions (e.g. closures, lambdas, loops, ...).
    fn priv_function_with_body_multi_lowering(
        &self,
        function_id: defs::ids::FunctionWithBodyId,
    ) -> Maybe<Arc<MultiLowering>>;

    /// Computes the lowered representation of a function with a body before borrow checking.
    fn priv_function_with_body_lowering(
        &self,
        function_id: ids::FunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

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

    /// Computes the lowered representation after the inlining phase.
    fn priv_concrete_function_with_body_postinline_lowered(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Computes the lowered representation after the panic phase.
    fn concrete_function_with_body_postpanic_lowered(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Computes the final lowered representation (after all the internal transformations).
    fn concrete_function_with_body_lowered(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Returns the set of direct callees of a concrete function with a body after the inline phase.
    fn concrete_function_with_body_postinline_direct_callees(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Maybe<Vec<ids::FunctionId>>;

    /// Returns the set of direct callees of a concrete function with a body after the panic phase.
    fn concrete_function_with_body_postpanic_direct_callees(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Maybe<Vec<ids::FunctionId>>;

    /// Returns the set of direct callees which are functions with body of a concrete function with
    /// a body (i.e. excluding libfunc callees), after the inline phase.
    fn concrete_function_with_body_postinline_direct_callees_with_body(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>>;

    /// Returns the set of direct callees which are functions with body of a concrete function with
    /// a body (i.e. excluding libfunc callees), after the panic phase.
    fn concrete_function_with_body_postpanic_direct_callees_with_body(
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

    /// Returns `true` if the function calls (possibly indirectly) itself, or if it calls (possibly
    /// indirectly) such a function. For example, if f0 calls f1, f1 calls f2, f2 calls f3, and f3
    /// calls f2, then [Self::contains_cycle] will return `true` for all of these functions.
    #[salsa::invoke(crate::graph_algorithms::cycles::contains_cycle)]
    #[salsa::cycle(crate::graph_algorithms::cycles::contains_cycle_handle_cycle)]
    fn contains_cycle(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
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
    /// This is using the representation after the panic phase.
    #[salsa::invoke(
        crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_scc_postpanic_representative
    )]
    fn concrete_function_with_body_scc_postpanic_representative(
        &self,
        function: ids::ConcreteFunctionWithBodyId,
        dependency_type: DependencyType,
    ) -> ConcreteSCCRepresentative;

    /// Returns all the concrete functions in the same strongly connected component as the given
    /// concrete function.
    /// This is using the representation after the panic phase.
    #[salsa::invoke(
        crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_postpanic_scc
    )]
    fn concrete_function_with_body_postpanic_scc(
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

    /// Returns the configuration struct that controls the behavior of the optimization passes.
    #[salsa::input]
    fn optimization_config(&self) -> Arc<OptimizationConfig>;
}

pub fn init_lowering_group(db: &mut (dyn LoweringGroup + 'static)) {
    let mut moveable_functions: Vec<String> =
        ["bool_not_impl", "felt252_add", "felt252_sub", "felt252_mul", "felt252_div"]
            .into_iter()
            .map(|s| s.to_string())
            .collect();

    for ty in ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "u128"] {
        moveable_functions.push(format!("integer::{}_wide_mul", ty));
    }

    db.set_optimization_config(Arc::new(OptimizationConfig::default().with_moveable_functions(moveable_functions)));
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
    let multi_lowering = lower_semantic_function(db.upcast(), function_id)?;
    Ok(Arc::new(multi_lowering))
}

// * Borrow checking.
fn priv_function_with_body_lowering(
    db: &dyn LoweringGroup,
    function_id: ids::FunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let semantic_function_id = function_id.base_semantic_function(db);
    let multi_lowering = db.priv_function_with_body_multi_lowering(semantic_function_id)?;
    let lowered = match &db.lookup_intern_lowering_function_with_body(function_id) {
        ids::FunctionWithBodyLongId::Semantic(_) => multi_lowering.main_lowering.clone(),
        ids::FunctionWithBodyLongId::Generated { element, .. } => {
            multi_lowering.generated_lowerings[element].clone()
        }
    };
    Ok(Arc::new(lowered))
}

fn function_with_body_lowering(
    db: &dyn LoweringGroup,
    function_id: ids::FunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let mut lowered = (*db.priv_function_with_body_lowering(function_id)?).clone();
    let module_file_id = function_id.base_semantic_function(db).module_file_id(db.upcast());
    borrow_check(db, module_file_id, &mut lowered);
    Ok(Arc::new(lowered))
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

// Applies inlining.
fn priv_concrete_function_with_body_postinline_lowered(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let mut lowered = (*db.priv_concrete_function_with_body_lowered_flat(function)?).clone();
    apply_inlining(db, function, &mut lowered)?;
    Ok(Arc::new(lowered))
}

// * Adds `withdraw_gas` calls.
// * Adds panics.
// * Adds destructor calls.
fn concrete_function_with_body_postpanic_lowered(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let mut lowered = (*db.priv_concrete_function_with_body_postinline_lowered(function)?).clone();

    add_withdraw_gas(db, function, &mut lowered)?;
    lowered = lower_panics(db, function, &lowered)?;
    return_optimization(db, &mut lowered);
    add_destructs(db, function, &mut lowered);
    Ok(Arc::new(lowered))
}

// * Optimizes remappings.
// * Delays var definitions.
// * Lowers implicits.
// * Optimizes matches.
// * Optimizes remappings again.
// * Reorganizes blocks (topological sort).
fn concrete_function_with_body_lowered(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let mut lowered = (*db.concrete_function_with_body_postpanic_lowered(function)?).clone();
    optimize_remappings(&mut lowered);
    // The call to `reorder_statements` before and after `branch_inversion` is intentional.
    // See description of `branch_inversion` for more details.
    reorder_statements(db, &mut lowered);
    branch_inversion(db, &mut lowered);
    reorder_statements(db, &mut lowered);
    const_folding(db, &mut lowered);
    optimize_matches(&mut lowered);
    lower_implicits(db, function, &mut lowered);
    optimize_remappings(&mut lowered);
    cancel_ops(&mut lowered);
    reorder_statements(db, &mut lowered);
    // `reorder_statements` may have caused some remappings to be redundant, so they need to be
    // removed.
    // `reorganize_blocks` assumes that there is no remappings on a goto to a block with 1 incoming
    // edge.
    // SierraGen drop additions assumes all remappings are of used variables.
    optimize_remappings(&mut lowered);
    reorganize_blocks(&mut lowered);

    Ok(Arc::new(lowered))
}

/// Given the lowering of a function, returns the set of direct dependencies of that function,
/// according to the given [DependencyType]. See [DependencyType] for more information about
/// what is considered a dependency.
pub(crate) fn get_direct_callees(
    db: &dyn LoweringGroup,
    lowered_function: &FlatLowered,
    dependency_type: DependencyType,
) -> Vec<ids::FunctionId> {
    // TODO(orizi): Follow calls for destructors as well.
    let mut direct_callees = Vec::new();
    if lowered_function.blocks.is_empty() {
        return direct_callees;
    }
    let withdraw_gas_fns = corelib::core_withdraw_gas_fns(db.upcast())
        .map(|id| db.intern_lowering_function(FunctionLongId::Semantic(id)));
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
                if dependency_type != DependencyType::Cost || statement_call.coupon_input.is_none()
                {
                    direct_callees.push(statement_call.function);
                }
            }
        }
        match &block.end {
            FlatBlockEnd::NotSet | FlatBlockEnd::Return(_) | FlatBlockEnd::Panic(_) => {}
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

fn concrete_function_with_body_postinline_direct_callees(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<Vec<ids::FunctionId>> {
    let lowered_function = db.priv_concrete_function_with_body_postinline_lowered(function_id)?;
    Ok(get_direct_callees(db, &lowered_function, dependency_type))
}

fn concrete_function_with_body_postpanic_direct_callees(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<Vec<ids::FunctionId>> {
    let lowered_function = db.concrete_function_with_body_postpanic_lowered(function_id)?;
    Ok(get_direct_callees(db, &lowered_function, dependency_type))
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
    let ids::FunctionLongId::Semantic(function_id) = concrete.lookup(db) else {
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
    let name = db.lookup_intern_extern_function(extern_function_id).name(db.upcast());
    if !(name == "coupon_buy" || name == "coupon_refund") {
        return Ok(None);
    }

    // Extract the coupon function from the generic argument.
    let [semantic::GenericArgumentId::Type(type_id)] = concrete_function.generic_args[..] else {
        panic!("Unexpected generic_args for coupon_buy().");
    };
    let semantic::TypeLongId::Coupon(coupon_function) = db.lookup_intern_type(type_id) else {
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

fn concrete_function_with_body_postinline_direct_callees_with_body(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>> {
    functions_with_body_from_function_ids(
        db,
        db.concrete_function_with_body_postinline_direct_callees(function_id, dependency_type)?,
        dependency_type,
    )
}

fn concrete_function_with_body_postpanic_direct_callees_with_body(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>> {
    functions_with_body_from_function_ids(
        db,
        db.concrete_function_with_body_postpanic_direct_callees(function_id, dependency_type)?,
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
            let location = Location {
                stable_location: function_id
                    .base_semantic_function(db)
                    .stable_location(db.upcast()),
                notes: vec![],
            };
            diagnostics.add(LoweringDiagnostic {
                location,
                kind: LoweringDiagnosticKind::NoPanicFunctionCycle,
            });
        }
    }

    diagnostics.extend(
        db.priv_inline_data(function_id)
            .map(|inline_data| inline_data.diagnostics.clone())
            .unwrap_or_default(),
    );

    Ok(diagnostics.build())
}

fn semantic_function_with_body_lowering_diagnostics(
    db: &dyn LoweringGroup,
    semantic_function_id: defs::ids::FunctionWithBodyId,
) -> Maybe<Diagnostics<LoweringDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();

    if let Ok(multi_lowering) = db.priv_function_with_body_multi_lowering(semantic_function_id) {
        let function_id = db.intern_lowering_function_with_body(
            ids::FunctionWithBodyLongId::Semantic(semantic_function_id),
        );
        diagnostics
            .extend(db.function_with_body_lowering_diagnostics(function_id).unwrap_or_default());
        for (element, _) in multi_lowering.generated_lowerings.iter() {
            let function_id =
                db.intern_lowering_function_with_body(ids::FunctionWithBodyLongId::Generated {
                    parent: semantic_function_id,
                    element: *element,
                });
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
            ModuleItemId::Trait(_) => {}
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
