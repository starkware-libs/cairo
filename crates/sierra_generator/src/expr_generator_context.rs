use std::collections::hash_map;

use defs::ids::{FreeFunctionId, HasName};
use smol_str::SmolStr;
use utils::unordered_hash_map::UnorderedHashMap;

use crate::db::SierraGenGroup;
use crate::id_allocator::IdAllocator;
use crate::pre_sierra;

/// Context for the methods that generate Sierra instructions for an expression.
pub struct ExprGeneratorContext<'a> {
    db: &'a dyn SierraGenGroup,
    function_id: FreeFunctionId,
    var_id_allocator: IdAllocator,
    label_id_allocator: IdAllocator,
    variables: UnorderedHashMap<defs::ids::VarId, sierra::ids::VarId>,
}
impl<'a> ExprGeneratorContext<'a> {
    /// Constructs an empty [ExprGeneratorContext].
    pub fn new(db: &'a dyn SierraGenGroup, function_id: FreeFunctionId) -> Self {
        ExprGeneratorContext {
            db,
            function_id,
            var_id_allocator: IdAllocator::default(),
            label_id_allocator: IdAllocator::default(),
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

    /// Attaches a local variable with its Sierra variable.
    /// See [Self::get_variable].
    pub fn register_variable(
        &mut self,
        local_var_id: defs::ids::VarId,
        sierra_var: sierra::ids::VarId,
    ) {
        match self.variables.entry(local_var_id) {
            hash_map::Entry::Occupied(_) => {
                // TODO(lior): Either panic!() if this is necessarily an internal compiler error
                //   or use diagnostics or Result.
                unimplemented!()
            }
            hash_map::Entry::Vacant(entry) => entry.insert(sierra_var),
        };
    }

    /// Returns the Sierra variable associated with the given local variable.
    /// See [Self::register_variable].
    pub fn get_variable(&self, local_var: defs::ids::VarId) -> sierra::ids::VarId {
        // TODO(lior): Consider throwing an error with a location.
        self.variables.get(&local_var).expect("Internal compiler error.").clone()
    }

    /// Generates a label id and a label statement.
    // TODO(lior): Consider using stable ids, instead of allocating sequential ids.
    pub fn new_label(&mut self) -> (pre_sierra::Statement, pre_sierra::LabelId) {
        let id = self.get_db().intern_label_id(pre_sierra::LabelLongId {
            parent: self.function_id,
            id: self.label_id_allocator.allocate(),
        });
        (pre_sierra::Statement::Label(pre_sierra::Label { id }), id)
    }

    fn get_extension_id_without_generics(
        &self,
        name: impl Into<SmolStr>,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string(name),
            args: vec![],
        })
    }

    pub fn felt_const_libfunc_id(&self, value: usize) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string("felt_const"),
            args: vec![sierra::program::GenericArg::Value(value as i64)],
        })
    }

    pub fn store_temp_libfunc_id(&self, ty: semantic::TypeId) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string("store_temp"),
            args: vec![sierra::program::GenericArg::Type(self.db.intern_type_id(ty))],
        })
    }

    pub fn function_call_libfunc_id(
        &self,
        func: semantic::ConcreteFunctionId,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.db.intern_concrete_lib_func(sierra::program::ConcreteLibFuncLongId {
            generic_id: sierra::ids::GenericLibFuncId::from_string("function_call"),
            args: vec![sierra::program::GenericArg::UserFunc(self.db.intern_function(func))],
        })
    }

    pub fn jump_nz_libfunc_id(&self) -> sierra::ids::ConcreteLibFuncId {
        self.get_extension_id_without_generics("jump_nz")
    }

    pub fn jump_libfunc_id(&self) -> sierra::ids::ConcreteLibFuncId {
        self.get_extension_id_without_generics("jump")
    }

    pub fn generic_libfunc_id(
        &self,
        extern_id: defs::ids::ExternFunctionId,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.get_extension_id_without_generics(extern_id.name(self.get_db().as_defs_group()))
    }
}
