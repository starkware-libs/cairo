use std::collections::HashSet;

use defs::ids::{FreeFunctionId, GenericFunctionId};
use diagnostics::Maybe;
use itertools::Itertools;
use semantic::TypeId;
use utils::strongly_connected_components::{compute_scc, GraphNode};

use crate::db::{LoweringGroup, SCCRepresentative};

/// Query implementation of [crate::db::LoweringGroup::function_scc_representative].
pub fn function_scc_representative(
    db: &dyn LoweringGroup,
    function: FreeFunctionId,
) -> SCCRepresentative {
    SCCRepresentative(db.function_scc(function).into_iter().min().unwrap_or(function))
}

/// Query implementation of [crate::db::LoweringGroup::function_scc_explicit_implicits].
pub fn function_scc_explicit_implicits(
    db: &dyn LoweringGroup,
    function: SCCRepresentative,
) -> Maybe<HashSet<TypeId>> {
    let scc = function_scc(db, function.0);
    let mut explicit_implicits = HashSet::new();
    for func in scc {
        let current_implicits: HashSet<TypeId> =
            db.free_function_declaration_implicits(func)?.into_iter().collect();
        explicit_implicits.extend(current_implicits);
    }
    Ok(explicit_implicits)
}

/// Query implementation of [crate::db::LoweringGroup::function_all_implicits].
pub fn function_all_implicits(
    db: &dyn LoweringGroup,
    function: semantic::FunctionId,
) -> Maybe<Vec<TypeId>> {
    match db.lookup_intern_function(function).function.generic_function {
        GenericFunctionId::Free(free_function) => db.free_function_all_implicits_vec(free_function),
        GenericFunctionId::Extern(extern_function) => {
            db.extern_function_declaration_implicits(extern_function)
        }
        GenericFunctionId::TraitFunction(_) | GenericFunctionId::ImplFunction(_) => todo!(),
    }
}

/// Query implementation of [crate::db::LoweringGroup::free_function_all_implicits].
pub fn free_function_all_implicits(
    db: &dyn LoweringGroup,
    function: FreeFunctionId,
) -> Maybe<HashSet<TypeId>> {
    // Find the SCC representative.
    let scc_representative = db.function_scc_representative(function);

    // Start with the explicit implicits of the SCC.
    let mut all_implicits = db.function_scc_explicit_implicits(scc_representative.clone())?;

    // For each direct callee, add its implicits.
    for direct_callee in db.free_function_definition_direct_callees(function)? {
        let current_implicits =
            match db.lookup_intern_function(direct_callee).function.generic_function {
                GenericFunctionId::Free(free_function) => {
                    // For a free function, call this method recursively. To avoid cycles, first
                    // check that the callee is not in this function's SCC.
                    let direct_callee_representative =
                        db.function_scc_representative(free_function);
                    if direct_callee_representative == scc_representative {
                        // We already have the implicits of this SCC - do nothing.
                        continue;
                    }
                    db.free_function_all_implicits(direct_callee_representative.0)?
                }
                GenericFunctionId::Extern(extern_function) => {
                    // All implicits of a libfunc are explicit implicits.
                    db.extern_function_declaration_implicits(extern_function)?.into_iter().collect()
                }
                GenericFunctionId::TraitFunction(_) | GenericFunctionId::ImplFunction(_) => todo!(),
            };
        all_implicits.extend(&current_implicits);
    }
    Ok(all_implicits)
}

/// Query implementation of [crate::db::LoweringGroup::free_function_all_implicits_vec].
pub fn free_function_all_implicits_vec(
    db: &dyn LoweringGroup,
    function: FreeFunctionId,
) -> Maybe<Vec<TypeId>> {
    let implicits_set = db.free_function_all_implicits(function)?;
    let mut implicits_vec = implicits_set.into_iter().collect_vec();

    let semantic_db = db.upcast();
    let precedence = db.implicit_precedence();
    implicits_vec.sort_by_cached_key(|type_id| {
        if let Some(idx) = precedence.iter().position(|item| item == type_id) {
            return (idx, "".to_string());
        }

        (precedence.len(), type_id.format(semantic_db))
    });

    Ok(implicits_vec)
}

/// Query implementation of [crate::db::LoweringGroup::function_scc].
pub fn function_scc(db: &dyn LoweringGroup, function_id: FreeFunctionId) -> Vec<FreeFunctionId> {
    compute_scc::<FreeFunctionNode<'_>>(FreeFunctionNode {
        free_function_id: function_id,
        db: db.upcast(),
    })
}

/// A node to use in the SCC computation.
#[derive(Clone)]
struct FreeFunctionNode<'a> {
    free_function_id: FreeFunctionId,
    db: &'a dyn LoweringGroup,
}
impl<'a> GraphNode for FreeFunctionNode<'a> {
    type NodeId = FreeFunctionId;

    fn get_neighbors(&self) -> Vec<Self> {
        self.db
            .free_function_definition_direct_free_function_callees(self.free_function_id)
            .unwrap()
            .into_iter()
            .map(|free_function_id| FreeFunctionNode { free_function_id, db: self.db })
            .collect()
    }

    fn get_id(&self) -> Self::NodeId {
        self.free_function_id
    }
}

/// Query implementation of [crate::db::LoweringGroup::function_may_panic].
pub fn function_may_panic(db: &dyn LoweringGroup, function: semantic::FunctionId) -> Maybe<bool> {
    match db.lookup_intern_function(function).function.generic_function {
        GenericFunctionId::Free(free_function) => db.free_function_may_panic(free_function),
        GenericFunctionId::Extern(extern_function) => {
            Ok(db.extern_function_declaration_signature(extern_function)?.panicable)
        }
        GenericFunctionId::TraitFunction(_) | GenericFunctionId::ImplFunction(_) => todo!(),
    }
}

/// Query implementation of [crate::db::LoweringGroup::free_function_may_panic].
pub fn free_function_may_panic(
    db: &dyn LoweringGroup,
    free_function: FreeFunctionId,
) -> Maybe<bool> {
    // Find the SCC representative.
    let scc_representative = db.function_scc_representative(free_function);

    // TODO(spapini): Add something that actually panics.
    // For each direct callee, find if it may panic.
    for direct_callee in db.free_function_definition_direct_callees(free_function)? {
        match db.lookup_intern_function(direct_callee).function.generic_function {
            GenericFunctionId::Free(free_function) => {
                // For a free function, call this method recursively. To avoid cycles, first
                // check that the callee is not in this function's SCC.
                let direct_callee_representative = function_scc_representative(db, free_function);
                if direct_callee_representative == scc_representative {
                    // We already have the implicits of this SCC - do nothing.
                    continue;
                }
                if db.free_function_may_panic(direct_callee_representative.0)? {
                    return Ok(true);
                }
            }
            GenericFunctionId::Extern(extern_function) => {
                if db.extern_function_declaration_signature(extern_function)?.panicable {
                    return Ok(true);
                }
            }
            GenericFunctionId::TraitFunction(_) | GenericFunctionId::ImplFunction(_) => todo!(),
        };
    }
    Ok(false)
}
