#[cfg(test)]
#[path = "canonical_id_replacer_test.rs"]
mod test;

use std::collections::HashMap;

use sierra::ids::{ConcreteLibFuncId, ConcreteTypeId, FunctionId};

use crate::replace_ids::SierraIdReplacer;

#[derive(Default)]
struct CanonicalReplacer {
    type_ids: HashMap<ConcreteTypeId, ConcreteTypeId>,
    function_ids: HashMap<FunctionId, FunctionId>,
    libfunc_ids: HashMap<ConcreteLibFuncId, ConcreteLibFuncId>,
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

pub fn canonicalize_sierra_ids_in_program(
    program: &sierra::program::Program,
) -> sierra::program::Program {
    let mut program = program.clone();

    let mut replacer = CanonicalReplacer::default();

    for type_declaration in &mut program.type_declarations {
        let new_id = ConcreteTypeId::from_usize(replacer.type_ids.len());
        replacer.type_ids.insert(type_declaration.id.clone(), new_id.clone());
        type_declaration.id = new_id;
        replacer.replace_generic_args(&mut type_declaration.long_id.generic_args);
    }

    for function in &mut program.funcs {
        let new_id = FunctionId::from_usize(replacer.function_ids.len());
        replacer.function_ids.insert(function.id.clone(), new_id.clone());
        function.id = new_id;
    }

    for libfunc_declaration in &mut program.libfunc_declarations {
        let new_id = ConcreteLibFuncId::from_usize(replacer.libfunc_ids.len());
        replacer.libfunc_ids.insert(libfunc_declaration.id.clone(), new_id.clone());
        libfunc_declaration.id = new_id;
        replacer.replace_generic_args(&mut libfunc_declaration.long_id.generic_args);
    }

    for statement in &mut program.statements {
        if let sierra::program::GenStatement::Invocation(p) = statement {
            p.libfunc_id = replacer.replace_libfunc_id(&p.libfunc_id);
        }
    }

    program
}
