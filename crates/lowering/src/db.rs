use std::collections::HashSet;
use std::sync::Arc;

use db_utils::Upcast;
use defs::ids::{FreeFunctionId, ModuleId, ModuleItemId};
use diagnostics::{Diagnostics, DiagnosticsBuilder};
use filesystem::ids::FileId;
use semantic::db::SemanticGroup;
use semantic::TypeId;

use crate::diagnostic::LoweringDiagnostic;
use crate::lower::{lower, Lowered};

// Salsa database interface.
#[salsa::query_group(LoweringDatabase)]
pub trait LoweringGroup: SemanticGroup + Upcast<dyn SemanticGroup> {
    /// Computed the lowered representation of a free function.
    fn free_function_lowered(&self, free_function: FreeFunctionId) -> Option<Arc<Lowered>>;

    /// Aggregates module level semantic diagnostics.
    fn module_lowering_diagnostics(
        &self,
        module_id: ModuleId,
    ) -> Option<Diagnostics<LoweringDiagnostic>>;

    /// Aggregates file level lowering diagnostics.
    fn file_lowering_diagnostics(&self, file_id: FileId)
    -> Option<Diagnostics<LoweringDiagnostic>>;

    // --- Queries related to implicits ---

    /// Returns the representative of the function's strongly connected component. The
    /// representative is consistently chosen for all the functions in the same SCC.
    #[salsa::invoke(crate::lower::function_scc_representative)]
    fn function_scc_representative(&self, function: FreeFunctionId) -> FreeFunctionId;

    /// Returns the explicit implicits required by all the functions in the SCC of this function.
    /// These are all the implicit parameters that are explicitly declared in the functions of
    /// the given function's SCC.
    ///
    /// For better caching, this function should be called only with the representative of the SCC.
    #[salsa::invoke(crate::lower::function_scc_explicit_implicits)]
    fn function_scc_explicit_implicits(&self, function: FreeFunctionId) -> Option<HashSet<TypeId>>;

    /// Returns all the implicit parameters that the function requires (according to both its
    /// signature and the functions it calls). The items in the returned vector are unique and the
    /// order is consistent, but not necessarily related to the order of the explicit implicits in
    /// the signature of the function.
    #[salsa::invoke(crate::lower::function_all_implicits)]
    fn function_all_implicits(&self, function: semantic::FunctionId) -> Option<Vec<TypeId>>;

    /// Returns all the implicit parameters that the free function requires (according to both its
    /// signature and the functions it calls).
    #[salsa::invoke(crate::lower::free_function_all_implicits)]
    fn free_function_all_implicits(&self, function: FreeFunctionId) -> Option<HashSet<TypeId>>;

    /// Returns all the implicit parameters that the free function requires (according to both its
    /// signature and the functions it calls). The items in the returned vector are unique and the
    /// order is consistent, but not necessarily related to the order of the explicit implicits in
    /// the signature of the function.
    #[salsa::invoke(crate::lower::free_function_all_implicits_vec)]
    fn free_function_all_implicits_vec(&self, function: FreeFunctionId) -> Option<Vec<TypeId>>;

    /// Returns all the functions in the same strongly connected component as the given function.
    #[salsa::invoke(crate::lower::function_scc)]
    fn function_scc(&self, function_id: FreeFunctionId) -> Vec<FreeFunctionId>;
}

fn free_function_lowered(
    db: &dyn LoweringGroup,
    free_function_id: FreeFunctionId,
) -> Option<Arc<Lowered>> {
    Some(Arc::new(lower(db.upcast(), free_function_id)?))
}

fn module_lowering_diagnostics(
    db: &dyn LoweringGroup,
    module_id: ModuleId,
) -> Option<Diagnostics<LoweringDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for (_name, item) in db.module_items(module_id)?.items.iter() {
        match item {
            ModuleItemId::FreeFunction(free_function) => {
                diagnostics.extend(
                    db.free_function_lowered(*free_function)
                        .map(|lowered| lowered.diagnostics.clone())
                        .unwrap_or_default(),
                );
            }
            ModuleItemId::Submodule(_) => {}
            ModuleItemId::Use(_) => {}
            ModuleItemId::Struct(_) => {}
            ModuleItemId::Enum(_) => {}
            ModuleItemId::Trait(_) => {}
            ModuleItemId::Impl(_) => {}
            ModuleItemId::ExternType(_) => {}
            ModuleItemId::ExternFunction(_) => {}
        }
    }
    Some(diagnostics.build())
}

fn file_lowering_diagnostics(
    db: &dyn LoweringGroup,
    file_id: FileId,
) -> Option<Diagnostics<LoweringDiagnostic>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for module_id in db.file_modules(file_id)? {
        if let Some(module_diagnostics) = db.module_lowering_diagnostics(module_id) {
            diagnostics.extend(module_diagnostics)
        }
    }
    Some(diagnostics.build())
}
