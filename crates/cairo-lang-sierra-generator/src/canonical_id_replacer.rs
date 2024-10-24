#[cfg(test)]
#[path = "canonical_id_replacer_test.rs"]
mod test;

use std::collections::HashMap;

use cairo_lang_sierra::ids::{ConcreteLibfuncId, ConcreteTypeId, FunctionId};

use crate::replace_ids::SierraIdReplacer;

#[derive(Default)]
pub struct CanonicalReplacer {
    type_ids: HashMap<ConcreteTypeId, u64>,
    function_ids: HashMap<FunctionId, u64>,
    libfunc_ids: HashMap<ConcreteLibfuncId, u64>,
}

/// A replacer that replace the Ids in the program with canonical ones.
/// The canonical ids are defined by the order of the declaration in the program.
/// The first type_id is 0, the second type id is 1, etc.
impl CanonicalReplacer {
    /// Builds a replacer from a program.
    pub fn from_program(program: &cairo_lang_sierra::program::Program) -> Self {
        let mut type_ids = HashMap::default();

        for type_declaration in &program.type_declarations {
            type_ids.insert(type_declaration.id.clone(), type_ids.len() as u64);
        }

        let mut function_ids = HashMap::default();
        for function in &program.funcs {
            function_ids.insert(function.id.clone(), function_ids.len() as u64);
        }

        let mut libfunc_ids = HashMap::default();
        for libfunc_declaration in &program.libfunc_declarations {
            libfunc_ids.insert(libfunc_declaration.id.clone(), libfunc_ids.len() as u64);
        }

        Self { type_ids, function_ids, libfunc_ids }
    }
}

impl SierraIdReplacer for CanonicalReplacer {
    fn replace_libfunc_id(
        &self,
        id: &cairo_lang_sierra::ids::ConcreteLibfuncId,
    ) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
        cairo_lang_sierra::ids::ConcreteLibfuncId {
            id: *self.libfunc_ids.get(id).expect("Unexpected libfunc id."),
            debug_name: id.debug_name.clone(),
        }
    }

    fn replace_type_id(
        &self,
        id: &cairo_lang_sierra::ids::ConcreteTypeId,
    ) -> cairo_lang_sierra::ids::ConcreteTypeId {
        cairo_lang_sierra::ids::ConcreteTypeId {
            id: *self.type_ids.get(id).expect("Unexpected type id."),
            debug_name: id.debug_name.clone(),
        }
    }

    fn replace_function_id(
        &self,
        id: &cairo_lang_sierra::ids::FunctionId,
    ) -> cairo_lang_sierra::ids::FunctionId {
        cairo_lang_sierra::ids::FunctionId {
            id: *self.function_ids.get(id).expect("Unexpected function id."),
            debug_name: id.debug_name.clone(),
        }
    }
}
