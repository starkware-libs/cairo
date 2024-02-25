use std::sync::Arc;

use cairo_lang_defs as defs;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId, ModuleItemId};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::TypeId;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::Upcast;
use itertools::Itertools;

use crate::add_withdraw_gas::add_withdraw_gas;
use crate::borrow_check::borrow_check;
use crate::concretize::concretize_lowered;
use crate::destructs::add_destructs;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind};
use crate::graph_algorithms::feedback_set::flag_add_withdraw_gas;
use crate::inline::get_inline_diagnostics;
use crate::lower::{lower_semantic_function, MultiLowering};
use crate::optimizations::config::OptimizationConfig;
use crate::optimizations::strategy::{OptimizationStrategy, OptimizationStrategyId};
use crate::panic::lower_panics;
use crate::{ids, FlatBlockEnd, FlatLowered, Location, MatchInfo, Statement};

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

    /// Computes the final lowered representation (after all the internal transformations).
    fn final_concrete_function_with_body_lowered(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Returns the set of direct callees of a concrete function with a body after the inline phase.
    fn concrete_function_with_body_direct_callees(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Vec<ids::FunctionId>>;

    /// Returns the set of direct callees of a concrete function with a body after the panic phase.
    fn concrete_function_with_body_postpanic_direct_callees(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Vec<ids::FunctionId>>;

    /// Returns the set of direct callees which are functions with body of a concrete function with
    /// a body (i.e. excluding libfunc callees), after the inline phase.
    fn concrete_function_with_body_direct_callees_with_body(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>>;

    /// Returns the set of direct callees which are functions with body of a concrete function with
    /// a body (i.e. excluding libfunc callees), after the panic phase.
    fn concrete_function_with_body_postpanic_direct_callees_with_body(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
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
    ) -> Maybe<OrderedHashSet<ids::FunctionId>>;
    /// Returns the set of direct callees which are functions with body of a function with a body
    /// (i.e. excluding libfunc callees).
    #[salsa::invoke(
        crate::graph_algorithms::cycles::function_with_body_direct_function_with_body_callees
    )]
    fn function_with_body_direct_function_with_body_callees(
        &self,
        function_id: ids::FunctionWithBodyId,
    ) -> Maybe<OrderedHashSet<ids::FunctionWithBodyId>>;

    /// Returns `true` if the function calls (possibly indirectly) itself, or if it calls (possibly
    /// indirectly) such a function. For example, if f0 calls f1, f1 calls f2, f2 calls f3, and f3
    /// calls f2, then [Self::contains_cycle] will return `true` for all of these functions.
    #[salsa::invoke(crate::graph_algorithms::cycles::contains_cycle)]
    #[salsa::cycle(crate::graph_algorithms::cycles::contains_cycle_handle_cycle)]
    fn contains_cycle(&self, function_id: ids::ConcreteFunctionWithBodyId) -> Maybe<bool>;

    /// Returns `true` if the function calls (possibly indirectly) itself. For example, if f0 calls
    /// f1, f1 calls f2, f2 calls f3, and f3 calls f2, then [Self::in_cycle] will return
    /// `true` for f2 and f3, but false for f0 and f1.
    #[salsa::invoke(crate::graph_algorithms::cycles::in_cycle)]
    fn in_cycle(&self, function_id: ids::FunctionWithBodyId) -> Maybe<bool>;

    // ### Strongly connected components ###

    /// Returns the representative of the concrete function's strongly connected component. The
    /// representative is consistently chosen for all the concrete functions in the same SCC.
    #[salsa::invoke(
        crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_scc_representative
    )]
    fn concrete_function_with_body_scc_representative(
        &self,
        function: ids::ConcreteFunctionWithBodyId,
    ) -> ConcreteSCCRepresentative;

    /// Returns all the concrete functions in the same strongly connected component as the given
    /// concrete function.
    #[salsa::invoke(
        crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_scc
    )]
    fn concrete_function_with_body_scc(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
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
    ) -> Vec<ids::ConcreteFunctionWithBodyId>;

    /// Returns all the functions in the same strongly connected component as the given function.
    #[salsa::invoke(crate::scc::function_with_body_scc)]
    fn function_with_body_scc(
        &self,
        function_id: ids::FunctionWithBodyId,
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

    // Internal query for a heuristic to decide if a given `function_id` should be inlined.
    #[salsa::invoke(crate::inline::priv_should_inline)]
    fn priv_should_inline(&self, function_id: ids::ConcreteFunctionWithBodyId) -> Maybe<bool>;

    /// Returns the configuration struct that controls the behavior of the optimization passes.
    #[salsa::input]
    fn optimization_config(&self) -> Arc<OptimizationConfig>;

    /// Returns the default optimization strategy.
    #[salsa::invoke(crate::optimizations::strategy::default_optimization_strategy)]
    fn default_optimization_strategy(&self) -> OptimizationStrategyId;
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

    db.set_optimization_config(Arc::new(
        OptimizationConfig::default().with_moveable_functions(moveable_functions),
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

    Ok(Arc::new(lowered))
}

/// Query implementation of [LoweringGroup::final_concrete_function_with_body_lowered].
fn optimized_concrete_function_with_body_lowered(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
    optimization_strategy: OptimizationStrategyId,
) -> Maybe<Arc<FlatLowered>> {
    Ok(Arc::new(optimization_strategy.apply_strategy(db, function)?))
}

/// Query implementation of [LoweringGroup::final_concrete_function_with_body_lowered].
fn final_concrete_function_with_body_lowered(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    db.optimized_concrete_function_with_body_lowered(function, db.default_optimization_strategy())
}

/// Given the lowering of a function, returns the set of direct callees of that function.
fn get_direct_callees(lowered_function: &FlatLowered) -> Vec<ids::FunctionId> {
    let mut direct_callees = Vec::new();
    for (_, block) in &lowered_function.blocks {
        for statement in &block.statements {
            if let Statement::Call(statement_call) = statement {
                direct_callees.push(statement_call.function);
            }
        }
        if let FlatBlockEnd::Match { info: MatchInfo::Extern(s) } = &block.end {
            direct_callees.push(s.function);
        }
    }
    direct_callees
}

fn concrete_function_with_body_direct_callees(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Vec<ids::FunctionId>> {
    let lowered_function = db.priv_concrete_function_with_body_lowered_flat(function_id)?;
    Ok(get_direct_callees(&lowered_function))
}

fn concrete_function_with_body_postpanic_direct_callees(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Vec<ids::FunctionId>> {
    let lowered_function = db.concrete_function_with_body_postpanic_lowered(function_id)?;
    Ok(get_direct_callees(&lowered_function))
}

/// Given a vector of FunctionIds returns the vector of FunctionWithBodyIds of the
/// ConcreteFunctionWithBodyIds
fn functions_with_body_from_function_ids(
    db: &dyn LoweringGroup,
    function_ids: Vec<ids::FunctionId>,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>> {
    Ok(function_ids
        .into_iter()
        .map(|concrete| concrete.body(db.upcast()))
        .collect::<Maybe<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect_vec())
}

fn concrete_function_with_body_direct_callees_with_body(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>> {
    functions_with_body_from_function_ids(
        db,
        db.concrete_function_with_body_direct_callees(function_id)?,
    )
}

fn concrete_function_with_body_postpanic_direct_callees_with_body(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>> {
    functions_with_body_from_function_ids(
        db,
        db.concrete_function_with_body_postpanic_direct_callees(function_id)?,
    )
}

fn function_with_body_lowering_diagnostics(
    db: &dyn LoweringGroup,
    function_id: ids::FunctionWithBodyId,
) -> Maybe<Diagnostics<LoweringDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();

    if let Ok(lowered) = db.function_with_body_lowering(function_id) {
        diagnostics.extend(lowered.diagnostics.clone());
        if flag_add_withdraw_gas(db) && !lowered.signature.panicable && db.in_cycle(function_id)? {
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
