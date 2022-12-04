#[cfg(test)]
#[path = "canonical_id_replacer_test.rs"]
mod test;

use std::collections::HashMap;

use sierra::ids::{ConcreteLibFuncId, ConcreteTypeId, FunctionId};

use crate::replace_ids::SierraIdReplacer;

#[derive(Default)]
pub struct CanonicalReplacer {
    type_ids: HashMap<ConcreteTypeId, ConcreteTypeId>,
    function_ids: HashMap<FunctionId, FunctionId>,
    libfunc_ids: HashMap<ConcreteLibFuncId, ConcreteLibFuncId>,
}

impl CanonicalReplacer {
    pub fn from_program(program: &sierra::program::Program) -> Self {
        let mut type_ids = HashMap::default();

        for type_declaration in &program.type_declarations {
            let new_id = ConcreteTypeId::from_usize(type_ids.len());
            type_ids.insert(type_declaration.id.clone(), new_id.clone());
        }

        let mut function_ids = HashMap::default();
        for function in &program.funcs {
            let new_id = FunctionId::from_usize(function_ids.len());
            function_ids.insert(function.id.clone(), new_id.clone());
        }

        let mut libfunc_ids = HashMap::default();
        for libfunc_declaration in &program.libfunc_declarations {
            let new_id = ConcreteLibFuncId::from_usize(libfunc_ids.len());
            libfunc_ids.insert(libfunc_declaration.id.clone(), new_id.clone());
        }

        Self { type_ids, function_ids, libfunc_ids }
    }
}

impl SierraIdReplacer for CanonicalReplacer {
    fn replace_libfunc_id(
        &self,
        id: &sierra::ids::ConcreteLibFuncId,
    ) -> sierra::ids::ConcreteLibFuncId {
        self.libfunc_ids.get(id).expect("Unexpected lib func id.").clone()
    }

    fn replace_type_id(&self, id: &sierra::ids::ConcreteTypeId) -> sierra::ids::ConcreteTypeId {
        self.type_ids.get(id).expect("Unexpected type id.").clone()
    }

    fn replace_function_id(&self, id: &sierra::ids::FunctionId) -> sierra::ids::FunctionId {
        self.function_ids.get(id).expect("Unexpected type id.").clone()
    }
}
