use std::collections::HashSet;
use std::ops::Deref;
use std::sync::Arc;

use cairo_lang_defs::ids::{FunctionWithBodyId, LanguageElementId, ModuleId, ModuleItemId};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::TypeId;
use cairo_lang_utils::Upcast;
use itertools::Itertools;
use semantic::items::functions::ConcreteFunctionWithBodyId;
use semantic::ConcreteFunction;

use crate::borrow_check::borrow_check;
use crate::concretize::concretize_lowered;
use crate::diagnostic::LoweringDiagnostic;
use crate::implicits::lower_implicits;
use crate::inline::{apply_inlining, PrivInlineData};
use crate::lower::lower;
use crate::optimizations::match_optimizer::optimize_matches;
use crate::optimizations::remappings::optimize_remappings;
use crate::panic::lower_panics;
use crate::reorganize_blocks::reorganize_blocks;
use crate::{FlatBlockEnd, FlatLowered, MatchInfo, Statement};

// Salsa database interface.
#[salsa::query_group(LoweringDatabase)]
pub trait LoweringGroup: SemanticGroup + Upcast<dyn SemanticGroup> {
    // Reports inlining diagnostics.
    #[salsa::invoke(crate::inline::priv_inline_data)]
    fn priv_inline_data(&self, function_id: FunctionWithBodyId) -> Maybe<Arc<PrivInlineData>>;

    /// Computes the lowered representation of a function with a body.
    fn priv_function_with_body_lowered_flat(
        &self,
        function_id: FunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// A concrete version of priv_function_with_body_lowered_flat
    fn priv_concrete_function_with_body_lowered_flat(
        &self,
        function_id: ConcreteFunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Computes the final lowered representation (after all the internal transformations).
    fn concrete_function_with_body_lowered(
        &self,
        function_id: ConcreteFunctionWithBodyId,
    ) -> Maybe<Arc<FlatLowered>>;

    /// Returns the set of direct callees of a concrete function with a body.
    fn concrete_function_with_body_direct_callees(
        &self,
        function_id: ConcreteFunctionWithBodyId,
    ) -> Maybe<Vec<ConcreteFunction>>;

    /// Returns the set of direct callees which are functions with body of a concrete function with
    /// a body (i.e. excluding libfunc callees).
    fn concrete_function_with_body_direct_callees_with_body(
        &self,
        function_id: ConcreteFunctionWithBodyId,
    ) -> Maybe<Vec<ConcreteFunctionWithBodyId>>;

    /// Aggregates function level semantic diagnostics.
    fn function_with_body_lowering_diagnostics(
        &self,
        function_id: FunctionWithBodyId,
    ) -> Maybe<Arc<Diagnostics<LoweringDiagnostic>>>;
    /// Aggregates module level semantic diagnostics.
    fn module_lowering_diagnostics(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Diagnostics<LoweringDiagnostic>>;

    /// Aggregates file level lowering diagnostics.
    fn file_lowering_diagnostics(&self, file_id: FileId) -> Maybe<Diagnostics<LoweringDiagnostic>>;

    // ### Queries related to implicits ###

    /// Returns the explicit implicits required by all the functions in the SCC of this function.
    /// These are all the implicit parameters that are explicitly declared in the functions of
    /// the given function's SCC.
    ///
    /// For better caching, this function should be called only with the representative of the SCC.
    #[salsa::invoke(crate::implicits::function_scc_explicit_implicits)]
    fn function_scc_explicit_implicits(
        &self,
        function: ConcreteSCCRepresentative,
    ) -> Maybe<HashSet<TypeId>>;

    /// Returns all the implicit parameters that the function requires (according to both its
    /// signature and the functions it calls). The items in the returned vector are unique and the
    /// order is consistent, but not necessarily related to the order of the explicit implicits in
    /// the signature of the function.
    #[salsa::invoke(crate::implicits::function_all_implicits)]
    fn function_all_implicits(&self, function: semantic::FunctionId) -> Maybe<Vec<TypeId>>;

    /// Returns all the implicit parameters that a concrete function with a body requires (according
    /// to both its signature and the functions it calls).
    #[salsa::invoke(crate::implicits::concrete_function_with_body_all_implicits)]
    fn concrete_function_with_body_all_implicits(
        &self,
        function: ConcreteFunctionWithBodyId,
    ) -> Maybe<HashSet<TypeId>>;

    /// Returns all the implicit parameters that a function with a body requires (according to both
    /// its signature and the functions it calls). The items in the returned vector are unique
    /// and the order is consistent, but not necessarily related to the order of the explicit
    /// implicits in the signature of the function.
    #[salsa::invoke(crate::implicits::concrete_function_with_body_all_implicits_vec)]
    fn concrete_function_with_body_all_implicits_vec(
        &self,
        function: ConcreteFunctionWithBodyId,
    ) -> Maybe<Vec<TypeId>>;

    /// An array that sets the precedence of implicit types.
    #[salsa::input]
    fn implicit_precedence(&self) -> Arc<Vec<TypeId>>;

    // ### Queries related to panics ###

    /// Returns whether the function may panic.
    #[salsa::invoke(crate::panic::function_may_panic)]
    fn function_may_panic(&self, function: semantic::FunctionId) -> Maybe<bool>;

    /// Returns whether the concrete function may panic.
    #[salsa::invoke(crate::panic::concrete_function_with_body_may_panic)]
    fn concrete_function_with_body_may_panic(
        &self,
        function: ConcreteFunctionWithBodyId,
    ) -> Maybe<bool>;

    /// Checks if the function has a block that ends with panic.
    #[salsa::invoke(crate::panic::has_direct_panic)]
    fn has_direct_panic(&self, function_id: ConcreteFunctionWithBodyId) -> Maybe<bool>;

    // ### cycles ###

    /// Returns `true` if the function calls (possibly indirectly) itself, or if it calls (possibly
    /// indirectly) such a function. For example, if f0 calls f1, f1 calls f2, f2 calls f3, and f3
    /// calls f2, then [Self::contains_cycle] will return `true` for all of these functions.
    #[salsa::invoke(crate::graph_algorithms::cycles::contains_cycle)]
    #[salsa::cycle(crate::graph_algorithms::cycles::contains_cycle_handle_cycle)]
    fn contains_cycle(&self, function_id: ConcreteFunctionWithBodyId) -> Maybe<bool>;

    /// Returns `true` if the function calls (possibly indirectly) itself. For example, if f0 calls
    /// f1, f1 calls f2, f2 calls f3, and f3 calls f2, then [Self::in_cycle] will return
    /// `true` for f2 and f3, but false for f0 and f1.
    #[salsa::invoke(crate::graph_algorithms::cycles::in_cycle)]
    fn in_cycle(&self, function_id: FunctionWithBodyId) -> Maybe<bool>;

    // ### Strongly connected components ###

    /// Returns the representative of the concrete function's strongly connected component. The
    /// representative is consistently chosen for all the concrete functions in the same SCC.
    #[salsa::invoke(
        crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_scc_representative
    )]
    fn concrete_function_with_body_scc_representative(
        &self,
        function: ConcreteFunctionWithBodyId,
    ) -> ConcreteSCCRepresentative;

    /// Returns all the concrete functions in the same strongly connected component as the given
    /// concrete function.
    #[salsa::invoke(
        crate::graph_algorithms::strongly_connected_components::concrete_function_with_body_scc
    )]
    fn concrete_function_with_body_scc(
        &self,
        function_id: ConcreteFunctionWithBodyId,
    ) -> Vec<ConcreteFunctionWithBodyId>;

    /// Returns the representative of the function's strongly connected component. The
    /// representative is consistently chosen for all the functions in the same SCC.
    #[salsa::invoke(crate::scc::function_scc_representative)]
    fn function_scc_representative(&self, function: FunctionWithBodyId)
    -> GenericSCCRepresentative;

    /// Returns all the functions in the same strongly connected component as the given function.
    #[salsa::invoke(crate::scc::function_with_body_scc)]
    fn function_with_body_scc(&self, function_id: FunctionWithBodyId) -> Vec<FunctionWithBodyId>;

    // ### Feedback set ###

    /// Returns the feedback-vertex-set of the given concrete function. A feedback-vertex-set is the
    /// set of vertices whose removal leaves a graph without cycles.
    #[salsa::invoke(crate::graph_algorithms::feedback_set::function_with_body_feedback_set)]
    fn function_with_body_feedback_set(
        &self,
        function: ConcreteFunctionWithBodyId,
    ) -> Maybe<HashSet<ConcreteFunctionWithBodyId>>;

    /// Returns the feedback-vertex-set of the given concrete-function SCC-representative. A
    /// feedback-vertex-set is the set of vertices whose removal leaves a graph without cycles.
    #[salsa::invoke(crate::graph_algorithms::feedback_set::priv_function_with_body_feedback_set_of_representative)]
    fn priv_function_with_body_feedback_set_of_representative(
        &self,
        function: ConcreteSCCRepresentative,
    ) -> Maybe<HashSet<ConcreteFunctionWithBodyId>>;
}

pub fn init_lowering_group(db: &mut (dyn LoweringGroup + 'static)) {
    // Initialize inputs.
    db.set_implicit_precedence(Arc::new(vec![]));
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct GenericSCCRepresentative(pub FunctionWithBodyId);

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct ConcreteSCCRepresentative(pub ConcreteFunctionWithBodyId);

// *** Main lowering phases in order.

// * Borrow checking.
fn priv_function_with_body_lowered_flat(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let mut lowered = lower(db.upcast(), function_id)?;
    borrow_check(function_id.module_file_id(db.upcast()), &mut lowered);
    Ok(Arc::new(lowered))
}

// * Concretizes lowered representation (monomorphization).
fn priv_concrete_function_with_body_lowered_flat(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let semantic_db = db.upcast();
    let mut lowered = (*db
        .priv_function_with_body_lowered_flat(function.function_with_body_id(semantic_db))?)
    .clone();
    concretize_lowered(db, &mut lowered, &function.substitution(semantic_db)?)?;
    Ok(Arc::new(lowered))
}

// * Applies inlining.
// * Adds panics.
// * Lowers implicits.
// * Optimize_matches
// * Topological sort.
// * Optimizes remappings
fn concrete_function_with_body_lowered(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let semantic_db = db.upcast();
    let mut lowered = (*db.priv_concrete_function_with_body_lowered_flat(function)?).clone();

    // TODO(spapini): passing function.function_with_body_id might be weird here.
    // It's not really needed for inlining, so try to remove.
    apply_inlining(db, function.function_with_body_id(semantic_db), &mut lowered)?;
    lowered = lower_panics(db, function, &lowered)?;
    lower_implicits(db, function, &mut lowered);
    optimize_matches(&mut lowered);
    optimize_remappings(&mut lowered);
    reorganize_blocks(&mut lowered);
    Ok(Arc::new(lowered))
}

fn concrete_function_with_body_direct_callees(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<Vec<ConcreteFunction>> {
    let mut direct_callees = Vec::new();
    let lowered_function =
        (*db.priv_concrete_function_with_body_lowered_flat(function_id)?).clone();
    for (_, block) in &lowered_function.blocks {
        for statement in &block.statements {
            if let Statement::Call(statement_call) = statement {
                let concrete = db.lookup_intern_function(statement_call.function).function;
                direct_callees.push(concrete);
            }
        }
        if let FlatBlockEnd::Match { info: MatchInfo::Extern(s) } = &block.end {
            direct_callees.push(s.function.get_concrete(db.upcast()));
        }
    }
    Ok(direct_callees)
}

fn concrete_function_with_body_direct_callees_with_body(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<Vec<ConcreteFunctionWithBodyId>> {
    Ok(db
        .concrete_function_with_body_direct_callees(function_id)?
        .into_iter()
        .map(|concrete| concrete.get_body(db.upcast()))
        .collect::<Maybe<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect_vec())
}

fn function_with_body_lowering_diagnostics(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Arc<Diagnostics<LoweringDiagnostic>>> {
    let mut diagnostics = DiagnosticsBuilder::default();

    diagnostics.extend(
        db.priv_function_with_body_lowered_flat(function_id)
            .map(|lowered| lowered.diagnostics.clone())
            .unwrap_or_default(),
    );

    diagnostics.extend(
        db.priv_inline_data(function_id)
            .map(|inline_data| inline_data.diagnostics.clone())
            .unwrap_or_default(),
    );

    Ok(Arc::new(diagnostics.build()))
}

fn module_lowering_diagnostics(
    db: &dyn LoweringGroup,
    module_id: ModuleId,
) -> Maybe<Diagnostics<LoweringDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for item in db.module_items(module_id)?.iter() {
        match item {
            ModuleItemId::FreeFunction(free_function) => {
                let function_id = FunctionWithBodyId::Free(*free_function);
                diagnostics.extend(
                    db.function_with_body_lowering_diagnostics(function_id)?.deref().clone(),
                );
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
                    let function_id = FunctionWithBodyId::Impl(*impl_func);
                    diagnostics.extend(
                        db.function_with_body_lowering_diagnostics(function_id)?.deref().clone(),
                    );
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
