use std::collections::HashMap;

use smol_str::SmolStr;

use crate::ids::{ConcreteLibFuncId, ConcreteTypeId, FunctionId};
use crate::program::{GenericArg, Program, Statement};

#[cfg(test)]
#[path = "debug_info_test.rs"]
mod test;

/// Debug information for a Sierra program, to get readable names.
#[derive(Debug, Eq, PartialEq)]
pub struct DebugInfo {
    pub type_names: HashMap<ConcreteTypeId, SmolStr>,
    pub libfunc_names: HashMap<ConcreteLibFuncId, SmolStr>,
    pub user_func_names: HashMap<FunctionId, SmolStr>,
}
impl DebugInfo {
    /// Extracts the existing debug info from a program.
    pub fn extract(program: &Program) -> Self {
        Self {
            type_names: program
                .type_declarations
                .iter()
                .filter_map(|decl| {
                    decl.id.debug_name.clone().map(|name| (ConcreteTypeId::new(decl.id.id), name))
                })
                .collect(),
            libfunc_names: program
                .libfunc_declarations
                .iter()
                .filter_map(|decl| {
                    decl.id
                        .debug_name
                        .clone()
                        .map(|name| (ConcreteLibFuncId::new(decl.id.id), name))
                })
                .collect(),
            user_func_names: program
                .funcs
                .iter()
                .filter_map(|func| {
                    func.id.debug_name.clone().map(|name| (FunctionId::new(func.id.id), name))
                })
                .collect(),
        }
    }

    /// Populates a program with debug info.
    pub fn populate(&self, program: &mut Program) {
        for decl in &mut program.type_declarations {
            self.try_replace_type_id(&mut decl.id);
            self.try_replace_generic_arg_ids(&mut decl.long_id.generic_args);
        }
        for decl in &mut program.libfunc_declarations {
            self.try_replace_libfunc_id(&mut decl.id);
            self.try_replace_generic_arg_ids(&mut decl.long_id.generic_args);
        }
        for func in &mut program.funcs {
            self.try_replace_function_id(&mut func.id);
            for param in &mut func.params {
                self.try_replace_type_id(&mut param.ty);
            }
            for id in &mut func.signature.param_types {
                self.try_replace_type_id(id);
            }
            for id in &mut func.signature.ret_types {
                self.try_replace_type_id(id);
            }
        }
        for statement in &mut program.statements {
            match statement {
                Statement::Invocation(invocation) => {
                    self.try_replace_libfunc_id(&mut invocation.libfunc_id)
                }
                Statement::Return(_) => {}
            }
        }
    }

    /// Replaces the debug names of the generic args if exists in the maps.
    fn try_replace_generic_arg_ids(&self, generic_args: &mut Vec<GenericArg>) {
        for generic_arg in generic_args {
            match generic_arg {
                GenericArg::Type(id) => self.try_replace_type_id(id),
                GenericArg::LibFunc(id) => self.try_replace_libfunc_id(id),
                GenericArg::UserFunc(id) => self.try_replace_function_id(id),
                GenericArg::Value(_) | GenericArg::UserType(_) => {}
            }
        }
    }

    /// Replaces the debug name of an id if exists in the matching map.
    fn try_replace_type_id(&self, id: &mut ConcreteTypeId) {
        if let Some(name) = self.type_names.get(id).cloned() {
            let _ = id.debug_name.insert(name);
        }
    }

    /// Replaces the debug name of an id if exists in the matching map.
    fn try_replace_libfunc_id(&self, id: &mut ConcreteLibFuncId) {
        if let Some(name) = self.libfunc_names.get(id).cloned() {
            let _ = id.debug_name.insert(name);
        }
    }

    /// Replaces the debug name of an id if exists in the matching map.
    fn try_replace_function_id(&self, id: &mut FunctionId) {
        if let Some(name) = self.user_func_names.get(id).cloned() {
            let _ = id.debug_name.insert(name);
        }
    }
}
