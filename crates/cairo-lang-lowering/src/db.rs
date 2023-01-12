use std::collections::HashSet;
use std::sync::Arc;

use cairo_lang_defs::ids::{
    FreeFunctionId, FunctionWithBodyId, LanguageElementId, ModuleId, ModuleItemId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::TypeId;
use cairo_lang_utils::Upcast;

use crate::borrow_check::borrow_check;
use crate::diagnostic::LoweringDiagnostic;
use crate::lower::lower;
use crate::panic::lower_panics;
use crate::{FlatLowered, StructuredLowered};

// Salsa database interface.
#[salsa::query_group(LoweringDatabase)]
pub trait LoweringGroup: SemanticGroup + Upcast<dyn SemanticGroup> {
    /// Computed the lowered representation of a free function.
    fn free_function_lowered_structured(
        &self,
        free_function: FreeFunctionId,
    ) -> Maybe<Arc<StructuredLowered>>;

    /// Computed the lowered representation of a free function.
    fn free_function_lowered_flat(&self, free_function: FreeFunctionId) -> Maybe<Arc<FlatLowered>>;

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

fn free_function_lowered_structured(
    db: &dyn LoweringGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Arc<StructuredLowered>> {
    Ok(Arc::new(lower(db.upcast(), free_function_id)?))
}

fn free_function_lowered_flat(
    db: &dyn LoweringGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Arc<FlatLowered>> {
    let structured = db.free_function_lowered_structured(free_function_id)?;
    let mut lowered = lower_panics(db, free_function_id, &structured)?;
    borrow_check(
        free_function_id.module_file_id(db.upcast()),
        free_function_id.stable_ptr(db.upcast()).untyped(),
        &mut lowered,
    );
    Ok(Arc::new(lowered))
}

fn module_lowering_diagnostics(
    db: &dyn LoweringGroup,
    module_id: ModuleId,
) -> Maybe<Diagnostics<LoweringDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for item in db.module_items(module_id)?.iter() {
        match item {
            ModuleItemId::FreeFunction(free_function) => {
                diagnostics.extend(
                    db.free_function_lowered_structured(*free_function)
                        .map(|lowered| lowered.diagnostics.clone())
                        .unwrap_or_default(),
                );
                diagnostics.extend(
                    db.free_function_lowered_flat(*free_function)
                        .map(|lowered| lowered.diagnostics.clone())
                        .unwrap_or_default(),
                );
            }
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
