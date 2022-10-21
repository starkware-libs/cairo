use std::sync::Arc;

use db_utils::Upcast;
use defs::ids::{FreeFunctionId, ModuleId, ModuleItemId};
use diagnostics::{Diagnostics, DiagnosticsBuilder};
use filesystem::ids::FileId;
use semantic::db::SemanticGroup;

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
