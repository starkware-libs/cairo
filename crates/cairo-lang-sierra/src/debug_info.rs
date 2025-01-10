use std::collections::HashMap;
use std::hash::Hash;

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::ids::{ConcreteLibfuncId, ConcreteTypeId, FunctionId};
use crate::program::{GenericArg, Program, Statement};

#[cfg(test)]
#[path = "debug_info_test.rs"]
mod test;

/// Debug information for a Sierra program, to get readable names.
#[derive(Clone, Debug, Eq, PartialEq, Default, Serialize, Deserialize)]
pub struct DebugInfo {
    #[serde(
        serialize_with = "serialize_map::<ConcreteTypeId, _>",
        deserialize_with = "deserialize_map::<ConcreteTypeId, _>"
    )]
    pub type_names: HashMap<ConcreteTypeId, SmolStr>,
    #[serde(
        serialize_with = "serialize_map::<ConcreteLibfuncId, _>",
        deserialize_with = "deserialize_map::<ConcreteLibfuncId, _>"
    )]
    pub libfunc_names: HashMap<ConcreteLibfuncId, SmolStr>,
    #[serde(
        serialize_with = "serialize_map::<FunctionId, _>",
        deserialize_with = "deserialize_map::<FunctionId, _>"
    )]
    pub user_func_names: HashMap<FunctionId, SmolStr>,
    /// Non-crucial information about the program, for use by external libraries and tools.
    ///
    /// See [`Annotations`] type documentation for more information about this field.
    #[serde(default, skip_serializing_if = "Annotations::is_empty")]
    pub annotations: Annotations,
    /// List of functions marked as executable.
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub executables: HashMap<SmolStr, Vec<FunctionId>>,
}

/// Store for non-crucial information about the program, for use by external libraries and tools.
///
/// Keys represent tool namespaces, and values are tool-specific annotations themselves.
/// Annotation values are JSON values, so they can be arbitrarily complex.
///
/// ## Namespaces
///
/// In order to avoid collisions between tools, namespaces should be URL-like, contain tool name.
/// It is not required for namespace URLs to exist, but it is preferable nonetheless.
///
/// A single tool might want to use multiple namespaces, for example to group together annotations
/// coming from different subcomponents of the tool. In such case, namespaces should use path-like
/// notation (e.g. `example.com/sub-namespace`).
///
/// For future-proofing, it might be a good idea to version namespaces, e.g. `example.com/v1`.
///
/// ### Example well-formed namespaces
///
/// - `scarb.swmansion.com`
/// - `scarb.swmansion.com/v1`
/// - `scarb.swmansion.com/build-info/v1`
pub type Annotations = OrderedHashMap<String, serde_json::Value>;

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
                        .map(|name| (ConcreteLibfuncId::new(decl.id.id), name))
                })
                .collect(),
            user_func_names: program
                .funcs
                .iter()
                .filter_map(|func| {
                    func.id.debug_name.clone().map(|name| (FunctionId::new(func.id.id), name))
                })
                .collect(),
            annotations: Default::default(),
            executables: Default::default(),
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
                GenericArg::Libfunc(id) => self.try_replace_libfunc_id(id),
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
    fn try_replace_libfunc_id(&self, id: &mut ConcreteLibfuncId) {
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

/// Trait for handling serde for the ids as map keys.
pub trait IdAsHashKey: Hash + Eq {
    /// Gets the inner id.
    fn get(&self) -> u64;
    /// Returns a new id from the given value.
    fn new(id: u64) -> Self;
}

impl IdAsHashKey for ConcreteTypeId {
    fn get(&self) -> u64 {
        self.id
    }

    fn new(id: u64) -> Self {
        Self::new(id)
    }
}
impl IdAsHashKey for ConcreteLibfuncId {
    fn get(&self) -> u64 {
        self.id
    }

    fn new(id: u64) -> Self {
        Self::new(id)
    }
}
impl IdAsHashKey for FunctionId {
    fn get(&self) -> u64 {
        self.id
    }

    fn new(id: u64) -> Self {
        Self::new(id)
    }
}

fn serialize_map<Id: IdAsHashKey, S: serde::Serializer>(
    m: &HashMap<Id, SmolStr>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    let v: Vec<_> = m.iter().map(|(id, name)| (id.get(), name)).sorted().collect();
    v.serialize(serializer)
}

fn deserialize_map<'de, Id: IdAsHashKey, D: serde::Deserializer<'de>>(
    deserializer: D,
) -> Result<HashMap<Id, SmolStr>, D::Error> {
    Ok(Vec::<(u64, SmolStr)>::deserialize(deserializer)?
        .into_iter()
        .map(|(id, name)| (Id::new(id), name))
        .collect())
}
