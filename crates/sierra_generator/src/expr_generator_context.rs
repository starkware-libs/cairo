use defs::diagnostic_utils::StableLocation;
use defs::ids::{FreeFunctionId, LanguageElementId, ModuleId};
use diagnostics::DiagnosticsBuilder;
use smol_str::SmolStr;
use syntax::node::ids::SyntaxStablePtrId;
use utils::unordered_hash_map::UnorderedHashMap;

use crate::db::SierraGenGroup;
use crate::diagnostic::SierraGeneratorDiagnosticKind;
use crate::id_allocator::IdAllocator;
use crate::{pre_sierra, SierraGeneratorDiagnostic};

/// Context for the methods that generate Sierra instructions for an expression.
pub struct ExprGeneratorContext<'a> {
    db: &'a dyn SierraGenGroup,
    // TODO(lior): Remove Option<> once the old expr_generator.rs is no longer used.
    lowered: Option<&'a lowering::lower::Lowered>,
    function_id: FreeFunctionId,
    module_id: ModuleId,
    diagnostics: &'a mut DiagnosticsBuilder<SierraGeneratorDiagnostic>,
    var_id_allocator: IdAllocator,
    label_id_allocator: IdAllocator,
    // TODO(lior): Remove old_variables once not used.
    old_variables: UnorderedHashMap<defs::ids::VarId, sierra::ids::VarId>,
    variables: UnorderedHashMap<lowering::VariableId, sierra::ids::VarId>,
}
impl<'a> ExprGeneratorContext<'a> {
    /// Constructs an empty [ExprGeneratorContext].
    pub fn new(
        db: &'a dyn SierraGenGroup,
        lowered: Option<&'a lowering::lower::Lowered>,
        function_id: FreeFunctionId,
        diagnostics: &'a mut DiagnosticsBuilder<SierraGeneratorDiagnostic>,
    ) -> Self {
        ExprGeneratorContext {
            db,
            lowered,
            function_id,
            module_id: function_id.module(db.upcast()),
            diagnostics,
            var_id_allocator: IdAllocator::default(),
            label_id_allocator: IdAllocator::default(),
            old_variables: UnorderedHashMap::default(),
            variables: UnorderedHashMap::default(),
        }
    }

    /// Allocates a new Sierra variable.
    pub fn allocate_sierra_variable(&mut self) -> sierra::ids::VarId {
        sierra::ids::VarId::from_usize(self.var_id_allocator.allocate())
    }

    /// Returns the SierraGenGroup salsa database.
    pub fn get_db(&self) -> &'a dyn SierraGenGroup {
        self.db
    }

    /// Returns the SierraGenGroup salsa database.
    pub fn function_id(&self) -> FreeFunctionId {
        self.function_id
    }

    /// Attaches a local variable with its Sierra variable.
    /// See [Self::get_variable].
    pub fn register_variable(
        &mut self,
        local_var_id: defs::ids::VarId,
        sierra_var: sierra::ids::VarId,
        stable_ptr: SyntaxStablePtrId,
    ) {
        if self.old_variables.insert(local_var_id, sierra_var).is_some() {
            self.add_diagnostic(
                SierraGeneratorDiagnosticKind::InternalErrorDuplicatedVariable,
                stable_ptr,
            );
        }
    }

    /// Returns the Sierra variable associated with the given local variable.
    /// See [Self::register_variable].
    pub fn get_variable(
        &mut self,
        local_var: defs::ids::VarId,
        stable_ptr: SyntaxStablePtrId,
    ) -> Option<sierra::ids::VarId> {
        let var = self.old_variables.get(&local_var);
        if var.is_none() {
            self.add_diagnostic(
                SierraGeneratorDiagnosticKind::InternalErrorUnknownVariable,
                stable_ptr,
            );
            return None;
        }
        var.cloned()
    }

    /// Returns the Sierra variable that corresponds to [lowering::VariableId].
    /// Allocates a new Sierra variable on the first call (for each variable).
    pub fn get_sierra_variable(&mut self, var: lowering::VariableId) -> sierra::ids::VarId {
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
        vars: &[lowering::VariableId],
    ) -> Vec<sierra::ids::VarId> {
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

    fn get_libfunc_id_without_generics(
        &self,
        name: impl Into<SmolStr>,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string(name),
            generic_args: vec![],
        })
    }

    fn get_libfunc_id_with_generic_arg(
        &self,
        name: impl Into<SmolStr>,
        ty: sierra::ids::ConcreteTypeId,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string(name),
            generic_args: vec![sierra::program::GenericArg::Type(ty)],
        })
    }

    pub fn felt_const_libfunc_id(&self, value: usize) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string("felt_const"),
            generic_args: vec![sierra::program::GenericArg::Value(value as i64)],
        })
    }

    pub fn function_call_libfunc_id(
        &self,
        func: semantic::FunctionId,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string("function_call"),
            generic_args: vec![sierra::program::GenericArg::UserFunc(
                self.db.intern_sierra_function(func),
            )],
        })
    }

    pub fn drop_libfunc_id(
        &self,
        ty: sierra::ids::ConcreteTypeId,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.get_libfunc_id_with_generic_arg("drop", ty)
    }

    pub fn dup_libfunc_id(
        &self,
        ty: sierra::ids::ConcreteTypeId,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.get_libfunc_id_with_generic_arg("dup", ty)
    }

    pub fn felt_jump_nz_libfunc_id(&self) -> sierra::ids::ConcreteLibFuncId {
        self.get_libfunc_id_without_generics("felt_jump_nz")
    }

    pub fn jump_libfunc_id(&self) -> sierra::ids::ConcreteLibFuncId {
        self.get_libfunc_id_without_generics("jump")
    }

    pub fn revoke_ap_tracking_libfunc_id(&self) -> sierra::ids::ConcreteLibFuncId {
        self.get_libfunc_id_without_generics("revoke_ap_tracking")
    }

    pub fn generic_libfunc_id(
        &self,
        extern_id: defs::ids::ExternFunctionId,
        generic_args: Vec<sierra::program::GenericArg>,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string(
                extern_id.name(self.db.upcast()),
            ),
            generic_args,
        })
    }

    /// Add a SierraGenerator diagnostic to the list of diagnostics.
    pub fn add_diagnostic(
        &mut self,
        kind: SierraGeneratorDiagnosticKind,
        stable_ptr: SyntaxStablePtrId,
    ) {
        self.diagnostics.add(SierraGeneratorDiagnostic {
            stable_location: StableLocation::new(self.module_id, stable_ptr),
            kind,
        });
    }

    /// Returns the [lowering::Variable] associated with [lowering::VariableId].
    pub fn get_lowered_variable(&self, var: lowering::VariableId) -> &'a lowering::Variable {
        &self.lowered.unwrap().variables[var]
    }

    /// Returns the block ([lowering::Block]) associated with [lowering::BlockId].
    pub fn get_lowered_block(&self, block_id: lowering::BlockId) -> &'a lowering::Block {
        &self.lowered.unwrap().blocks[block_id]
    }
}
