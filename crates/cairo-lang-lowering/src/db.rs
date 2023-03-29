use std::collections::HashSet;
use std::sync::Arc;

use cairo_lang_defs as defs;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId, ModuleItemId};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::TypeId;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::Upcast;
use itertools::Itertools;

use crate::add_withdraw_gas::add_withdraw_gas;
use crate::borrow_check::borrow_check;
use crate::concretize::concretize_lowered;
use crate::destructs::add_destructs;
use crate::diagnostic::LoweringDiagnostic;
use crate::implicits::lower_implicits;
use crate::inline::{apply_inlining, PrivInlineData};
use crate::lower::{lower, MultiLowering};
use crate::optimizations::delay_var_def::delay_var_def;
use crate::optimizations::match_optimizer::optimize_matches;
use crate::optimizations::remappings::optimize_remappings;
use crate::panic::lower_panics;
use crate::reorganize_blocks::reorganize_blocks;
use crate::{ids, FlatBlockEnd, FlatLowered, MatchInfo, Statement};

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

    // Reports inlining diagnostics.
    #[salsa::invoke(crate::inline::priv_inline_data)]
    fn priv_inline_data(&self, function_id: ids::FunctionWithBodyId) -> Maybe<Arc<PrivInlineData>>;

    /// Computes the lowered representation of a function with a body, along with all it generated
    /// functions (e.g. closures, lambdas, loops, ...).
    fn priv_function_with_body_multi_lowering(
        &self,
        function_id: defs::ids::FunctionWithBodyId,
    ) -> Maybe<Arc<MultiLowering>>;

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

    /// Computes the final lowered representation (after all the internal transformations).
    fn concrete_function_with_body_lowered(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Returns the set of direct callees of a concrete function with a body after the panic phase.
    fn concrete_function_with_body_postpanic_direct_callees(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Vec<ids::FunctionId>>;

    /// Returns the set of direct callees of a concrete function with a body.
    fn concrete_function_with_body_direct_callees(
        &self,
        function_id: ids::ConcreteFunctionWithBodyId,
    ) -> Maybe<Vec<ids::FunctionId>>;

    /// Returns the set of direct callees which are functions with body of a concrete function with
    /// a body (i.e. excluding libfunc callees).
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

    /// Returns all the implicitis used by a strongly connected component of functions.
    #[salsa::invoke(crate::implicits::scc_implicits)]
    fn scc_implicits(&self, function: ConcreteSCCRepresentative) -> Maybe<Vec<TypeId>>;

    /// An array that sets the precedence of implicit types.
    #[salsa::input]
    fn implicit_precedence(&self) -> Arc<Vec<TypeId>>;

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
    ) -> Maybe<HashSet<ids::ConcreteFunctionWithBodyId>>;

    /// Returns whether the given function needs an additional withdraw_gas call.
    #[salsa::invoke(crate::graph_algorithms::feedback_set::needs_withdraw_gas)]
    fn needs_withdraw_gas(&self, function: ids::ConcreteFunctionWithBodyId) -> Maybe<bool>;

    /// Returns the feedback-vertex-set of the given concrete-function SCC-representative. A
    /// feedback-vertex-set is the set of vertices whose removal leaves a graph without cycles.
    #[salsa::invoke(crate::graph_algorithms::feedback_set::priv_function_with_body_feedback_set_of_representative)]
    fn priv_function_with_body_feedback_set_of_representative(
        &self,
        function: ConcreteSCCRepresentative,
    ) -> Maybe<HashSet<ids::ConcreteFunctionWithBodyId>>;
}

pub fn init_lowering_group(db: &mut (dyn LoweringGroup + 'static)) {
    // Initialize inputs.
    db.set_implicit_precedence(Arc::new(vec![]));
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
    let multi_lowering = lower(db.upcast(), function_id)?;
    Ok(Arc::new(multi_lowering))
}

// * Borrow checking.
fn function_with_body_lowering(
    db: &dyn LoweringGroup,
    function_id: ids::FunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let semantic_function_id = function_id.semantic_function(db);
    let multi_lowering = db.priv_function_with_body_multi_lowering(semantic_function_id)?;
    let module_file_id = semantic_function_id.module_file_id(db.upcast());
    let mut lowering = match db.lookup_intern_lowering_function_with_body(function_id) {
        ids::FunctionWithBodyLongId::Semantic(_) => multi_lowering.main_lowering.clone(),
        ids::FunctionWithBodyLongId::Generated { element, .. } => {
            multi_lowering.generated_lowerings[element].clone()
        }
    };
    borrow_check(db, module_file_id, &mut lowering);
    Ok(Arc::new(lowering))
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

// * Applies inlining.
// * Adds withdraw_gas calls.
// * Adds panics.
// * Adds destructor calls.
fn concrete_function_with_body_postpanic_lowered(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let mut lowered = (*db.priv_concrete_function_with_body_lowered_flat(function)?).clone();

    apply_inlining(db, function, &mut lowered)?;
    add_withdraw_gas(db, function, &mut lowered)?;
    lowered = lower_panics(db, function, &lowered)?;
    add_destructs(db, function, &mut lowered);
    Ok(Arc::new(lowered))
}

// * Lowers implicits.
// * Optimize_matches
// * Topological sort.
// * Optimizes remappings
fn concrete_function_with_body_lowered(
    db: &dyn LoweringGroup,
    function: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let mut lowered = (*db.concrete_function_with_body_postpanic_lowered(function)?).clone();
    optimize_remappings(&mut lowered);
    delay_var_def(&mut lowered);
    lower_implicits(db, function, &mut lowered);
    optimize_matches(&mut lowered);
    optimize_remappings(&mut lowered);
    reorganize_blocks(&mut lowered);
    Ok(Arc::new(lowered))
}

fn concrete_function_with_body_direct_callees(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Vec<ids::FunctionId>> {
    let mut direct_callees = Vec::new();
    let lowered_function =
        (*db.priv_concrete_function_with_body_lowered_flat(function_id)?).clone();
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
    Ok(direct_callees)
}

fn concrete_function_with_body_postpanic_direct_callees(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Vec<ids::FunctionId>> {
    let mut direct_callees = Vec::new();
    let lowered_function =
        (*db.concrete_function_with_body_postpanic_lowered(function_id)?).clone();
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
    Ok(direct_callees)
}

fn concrete_function_with_body_direct_callees_with_body(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>> {
    Ok(db
        .concrete_function_with_body_direct_callees(function_id)?
        .into_iter()
        .map(|concrete| concrete.body(db.upcast()))
        .collect::<Maybe<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect_vec())
}

fn concrete_function_with_body_postpanic_direct_callees_with_body(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
) -> Maybe<Vec<ids::ConcreteFunctionWithBodyId>> {
    Ok(db
        .concrete_function_with_body_postpanic_direct_callees(function_id)?
        .into_iter()
        .map(|concrete| concrete.body(db.upcast()))
        .collect::<Maybe<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect_vec())
}

fn function_with_body_lowering_diagnostics(
    db: &dyn LoweringGroup,
    function_id: ids::FunctionWithBodyId,
) -> Maybe<Diagnostics<LoweringDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();

    if let Ok(lowered) = db.function_with_body_lowering(function_id) {
        diagnostics.extend(lowered.diagnostics.clone())
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
    for module_id in db.file_modules(file_id)? {
        if let Ok(module_diagnostics) = db.module_lowering_diagnostics(module_id) {
            diagnostics.extend(module_diagnostics)
        }
    }
    Ok(diagnostics.build())
}
