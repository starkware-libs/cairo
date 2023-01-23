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
use semantic::items::functions::ConcreteFunctionWithBodyId;

use crate::borrow_check::borrow_check;
use crate::concretize::concretize_lowered;
use crate::diagnostic::LoweringDiagnostic;
use crate::inline::{apply_inlining, PrivInlineData};
use crate::lower::lower;
use crate::panic::lower_panics;
use crate::{FlatLowered, StructuredLowered};

// Salsa database interface.
#[salsa::query_group(LoweringDatabase)]
pub trait LoweringGroup: SemanticGroup + Upcast<dyn SemanticGroup> {
    /// Computes the lowered representation of a function with a body.
    fn priv_function_with_body_lowered_structured(
        &self,
        function_id: FunctionWithBodyId,
    ) -> Maybe<Arc<StructuredLowered>>;

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

    // --- Queries related to implicits ---

    /// Returns the representative of the function's strongly connected component. The
    /// representative is consistently chosen for all the functions in the same SCC.
    #[salsa::invoke(crate::lower::implicits::function_scc_representative)]
    fn function_scc_representative(&self, function: FunctionWithBodyId) -> SCCRepresentative;

    /// Returns the explicit implicits required by all the functions in the SCC of this function.
    /// These are all the implicit parameters that are explicitly declared in the functions of
    /// the given function's SCC.
    ///
    /// For better caching, this function should be called only with the representative of the SCC.
    #[salsa::invoke(crate::lower::implicits::function_scc_explicit_implicits)]
    fn function_scc_explicit_implicits(
        &self,
        function: SCCRepresentative,
    ) -> Maybe<HashSet<TypeId>>;

    /// Returns all the implicit parameters that the function requires (according to both its
    /// signature and the functions it calls). The items in the returned vector are unique and the
    /// order is consistent, but not necessarily related to the order of the explicit implicits in
    /// the signature of the function.
    #[salsa::invoke(crate::lower::implicits::function_all_implicits)]
    fn function_all_implicits(&self, function: semantic::FunctionId) -> Maybe<Vec<TypeId>>;

    /// Returns all the implicit parameters that a function with a body requires (according to both
    /// its signature and the functions it calls).
    #[salsa::invoke(crate::lower::implicits::function_with_body_all_implicits)]
    fn function_with_body_all_implicits(
        &self,
        function: FunctionWithBodyId,
    ) -> Maybe<HashSet<TypeId>>;

    /// Returns all the implicit parameters that a function with a body requires (according to both
    /// its signature and the functions it calls). The items in the returned vector are unique
    /// and the order is consistent, but not necessarily related to the order of the explicit
    /// implicits in the signature of the function.
    #[salsa::invoke(crate::lower::implicits::function_with_body_all_implicits_vec)]
    fn function_with_body_all_implicits_vec(
        &self,
        function: FunctionWithBodyId,
    ) -> Maybe<Vec<TypeId>>;

    /// Returns whether the function may panic.
    #[salsa::invoke(crate::lower::implicits::function_may_panic)]
    fn function_may_panic(&self, function: semantic::FunctionId) -> Maybe<bool>;

    /// Returns whether the function may panic.
    #[salsa::invoke(crate::lower::implicits::function_with_body_may_panic)]
    fn function_with_body_may_panic(&self, function: FunctionWithBodyId) -> Maybe<bool>;

    /// Returns all the functions in the same strongly connected component as the given function.
    #[salsa::invoke(crate::lower::implicits::function_with_body_scc)]
    fn function_with_body_scc(&self, function_id: FunctionWithBodyId) -> Vec<FunctionWithBodyId>;

    /// An array that sets the precedence of implicit types.
    #[salsa::input]
    fn implicit_precedence(&self) -> Arc<Vec<TypeId>>;
}

pub fn init_lowering_group(db: &mut (dyn LoweringGroup + 'static)) {
    // Initialize inputs.
    db.set_implicit_precedence(Arc::new(vec![]));
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct SCCRepresentative(pub FunctionWithBodyId);

fn priv_function_with_body_lowered_structured(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Arc<StructuredLowered>> {
    Ok(Arc::new(lower(db.upcast(), function_id)?))
}

fn priv_function_with_body_lowered_flat(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let structured = db.priv_function_with_body_lowered_structured(function_id)?;
    let mut lowered = lower_panics(db, function_id, &structured)?;
    borrow_check(function_id.module_file_id(db.upcast()), &mut lowered);
    Ok(Arc::new(lowered))
}

fn priv_concrete_function_with_body_lowered_flat(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let semantic_db = db.upcast();
    let mut lowered = (*db
        .priv_function_with_body_lowered_flat(function.function_with_body_id(semantic_db))?)
    .clone();
    concretize_lowered(db, &mut lowered, &function.substitution(semantic_db)?);
    Ok(Arc::new(lowered))
}

fn concrete_function_with_body_lowered(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
) -> Maybe<Arc<FlatLowered>> {
    let semantic_db = db.upcast();
    let mut lowered = (*db.priv_concrete_function_with_body_lowered_flat(function)?).clone();

    // TODO(spapini): passing function.function_with_body_id might be weird here.
    // It's not really needed for inlining, so try to remove.
    apply_inlining(db, function.function_with_body_id(semantic_db), &mut lowered)?;
    Ok(Arc::new(lowered))
}

fn function_with_body_lowering_diagnostics(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Arc<Diagnostics<LoweringDiagnostic>>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    diagnostics.extend(
        db.priv_function_with_body_lowered_structured(function_id)
            .map(|lowered| lowered.diagnostics.clone())
            .unwrap_or_default(),
    );

    diagnostics.extend(
        db.priv_inline_data(function_id)
            .map(|inline_data| inline_data.diagnostics.clone())
            .unwrap_or_default(),
    );

    diagnostics.extend(
        db.priv_function_with_body_lowered_flat(function_id)
            .map(|lowered| lowered.diagnostics.clone())
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
            ModuleItemId::Impl(_) => {}
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
