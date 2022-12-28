use cairo_defs::diagnostic_utils::StableLocation;
use cairo_defs::ids::{FreeFunctionId, LanguageElementId, ModuleFileId};
use cairo_diagnostics::{DiagnosticsBuilder, Maybe};
use cairo_syntax::node::ids::SyntaxStablePtrId;
use cairo_utils::unordered_hash_map::UnorderedHashMap;

use crate::db::SierraGenGroup;
use crate::diagnostic::SierraGeneratorDiagnosticKind;
use crate::id_allocator::IdAllocator;
use crate::{pre_sierra, SierraGeneratorDiagnostic};

/// Context for the methods that generate Sierra instructions for an expression.
pub struct ExprGeneratorContext<'a> {
    db: &'a dyn SierraGenGroup,
    lowered: &'a cairo_lowering::lower::Lowered,
    function_id: FreeFunctionId,
    module_file_id: ModuleFileId,
    diagnostics: &'a mut DiagnosticsBuilder<SierraGeneratorDiagnostic>,
    var_id_allocator: IdAllocator,
    label_id_allocator: IdAllocator,
    variables: UnorderedHashMap<cairo_lowering::VariableId, cairo_sierra::ids::VarId>,
}
impl<'a> ExprGeneratorContext<'a> {
    /// Constructs an empty [ExprGeneratorContext].
    pub fn new(
        db: &'a dyn SierraGenGroup,
        lowered: &'a cairo_lowering::lower::Lowered,
        function_id: FreeFunctionId,
        diagnostics: &'a mut DiagnosticsBuilder<SierraGeneratorDiagnostic>,
    ) -> Self {
        ExprGeneratorContext {
            db,
            lowered,
            function_id,
            module_file_id: function_id.module_file(db.upcast()),
            diagnostics,
            var_id_allocator: IdAllocator::default(),
            label_id_allocator: IdAllocator::default(),
            variables: UnorderedHashMap::default(),
        }
    }

    /// Allocates a new Sierra variable.
    pub fn allocate_sierra_variable(&mut self) -> cairo_sierra::ids::VarId {
        cairo_sierra::ids::VarId::from_usize(self.var_id_allocator.allocate())
    }

    /// Returns the SierraGenGroup salsa database.
    pub fn get_db(&self) -> &'a dyn SierraGenGroup {
        self.db
    }

    /// Returns the Sierra variable that corresponds to [cairo_lowering::VariableId].
    /// Allocates a new Sierra variable on the first call (for each variable).
    pub fn get_sierra_variable(
        &mut self,
        var: cairo_lowering::VariableId,
    ) -> cairo_sierra::ids::VarId {
        if let Some(sierra_var) = self.variables.get(&var) {
            return sierra_var.clone();
        }

        let sierra_var = self.allocate_sierra_variable();
        self.variables.insert(var, sierra_var.clone());
        sierra_var
    }

    /// Same as [Self::get_sierra_variable] except that it operates of a list of variables.
    pub fn get_sierra_variables(
        &mut self,
        vars: &[cairo_lowering::VariableId],
    ) -> Vec<cairo_sierra::ids::VarId> {
        vars.iter().map(|var| self.get_sierra_variable(*var)).collect()
    }

    /// Generates a label id and a label statement.
    // TODO(lior): Consider using stable ids, instead of allocating sequential ids.
    pub fn new_label(&mut self) -> (pre_sierra::Statement, pre_sierra::LabelId) {
        let id = self.db.intern_label_id(pre_sierra::LabelLongId {
            parent: self.function_id,
            id: self.label_id_allocator.allocate(),
        });
        (pre_sierra::Statement::Label(pre_sierra::Label { id }), id)
    }

    /// Add a SierraGenerator diagnostic to the list of diagnostics.
    #[allow(dead_code)]
    pub fn add_diagnostic(
        &mut self,
        kind: SierraGeneratorDiagnosticKind,
        stable_ptr: SyntaxStablePtrId,
    ) {
        self.diagnostics.add(SierraGeneratorDiagnostic {
            stable_location: StableLocation::new(self.module_file_id, stable_ptr),
            kind,
        });
    }

    /// Returns the [cairo_sierra::ids::ConcreteTypeId] associated with
    /// [cairo_lowering::VariableId].
    pub fn get_variable_sierra_type(
        &self,
        var: cairo_lowering::VariableId,
    ) -> Maybe<cairo_sierra::ids::ConcreteTypeId> {
        self.db.get_concrete_type_id(self.lowered.variables[var].ty)
    }

    /// Returns the block ([cairo_lowering::Block]) associated with [cairo_lowering::BlockId].
    pub fn get_lowered_block(
        &self,
        block_id: cairo_lowering::BlockId,
    ) -> &'a cairo_lowering::Block {
        &self.lowered.blocks[block_id]
    }
}
