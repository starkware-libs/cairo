use std::cmp::Ordering;
use std::collections::HashSet;

use cairo_lang_defs::ids::{FunctionWithBodyId, UnstableSalsaId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::TypeId;
use cairo_lang_utils::strongly_connected_components::{compute_scc, GraphNode};
use itertools::Itertools;
use semantic::items::functions::GenericFunctionId;

use crate::db::{LoweringGroup, SCCRepresentative};

/// Query implementation of [crate::db::LoweringGroup::function_scc_representative].
pub fn function_scc_representative(
    db: &dyn LoweringGroup,
    function: FunctionWithBodyId,
) -> SCCRepresentative {
    SCCRepresentative(
        db.function_with_body_scc(function)
            .into_iter()
            .min_by(|x, y| match (x, y) {
                (FunctionWithBodyId::Free(x), FunctionWithBodyId::Free(y)) => {
                    x.get_internal_id().cmp(y.get_internal_id())
                }
                (FunctionWithBodyId::Impl(x), FunctionWithBodyId::Impl(y)) => {
                    x.get_internal_id().cmp(y.get_internal_id())
                }
                (FunctionWithBodyId::Free(_), FunctionWithBodyId::Impl(_)) => Ordering::Less,
                (FunctionWithBodyId::Impl(_), FunctionWithBodyId::Free(_)) => Ordering::Greater,
            })
            .unwrap_or(function),
    )
}

/// Query implementation of [crate::db::LoweringGroup::function_scc_explicit_implicits].
pub fn function_scc_explicit_implicits(
    db: &dyn LoweringGroup,
    function: SCCRepresentative,
) -> Maybe<HashSet<TypeId>> {
    let scc = function_with_body_scc(db, function.0);
    let mut explicit_implicits = HashSet::new();
    for func in scc {
        let current_implicits: HashSet<TypeId> = match func {
            FunctionWithBodyId::Free(free_function) => {
                db.free_function_declaration_implicits(free_function)?.into_iter().collect()
            }
            FunctionWithBodyId::Impl(impl_function) => {
                db.impl_function_declaration_implicits(impl_function)?.into_iter().collect()
            }
        };
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
        GenericFunctionId::Free(free_function) => {
            db.function_with_body_all_implicits_vec(FunctionWithBodyId::Free(free_function))
        }
        GenericFunctionId::Extern(extern_function) => {
            db.extern_function_declaration_implicits(extern_function)
        }
        GenericFunctionId::Impl(impl_function) => db
            .function_with_body_all_implicits_vec(FunctionWithBodyId::Impl(impl_function.function)),
        GenericFunctionId::Trait(_) => unreachable!(),
    }
}

/// Query implementation of [crate::db::LoweringGroup::function_with_body_all_implicits].
pub fn function_with_body_all_implicits(
    db: &dyn LoweringGroup,
    function: FunctionWithBodyId,
) -> Maybe<HashSet<TypeId>> {
    // Find the SCC representative.
    let scc_representative = db.function_scc_representative(function);

    // Start with the explicit implicits of the SCC.
    let mut all_implicits = db.function_scc_explicit_implicits(scc_representative.clone())?;

    // For each direct callee, add its implicits.
    for direct_callee in db.function_with_body_direct_callees(function)? {
        let current_implicits =
            match db.lookup_intern_function(direct_callee).function.generic_function {
                GenericFunctionId::Free(free_function) => {
                    // For a free function, call this method recursively. To avoid cycles, first
                    // check that the callee is not in this function's SCC.
                    let direct_callee_representative =
                        db.function_scc_representative(FunctionWithBodyId::Free(free_function));
                    if direct_callee_representative == scc_representative {
                        // We already have the implicits of this SCC - do nothing.
                        continue;
                    }
                    db.function_with_body_all_implicits(direct_callee_representative.0)?
                }
                GenericFunctionId::Impl(impl_function) => {
                    // For an impl function, call this method recursively. To avoid cycles, first
                    // check that the callee is not in this function's SCC.
                    let direct_callee_representative = db.function_scc_representative(
                        FunctionWithBodyId::Impl(impl_function.function),
                    );
                    if direct_callee_representative == scc_representative {
                        // We already have the implicits of this SCC - do nothing.
                        continue;
                    }
                    db.function_with_body_all_implicits(direct_callee_representative.0)?
                }
                GenericFunctionId::Extern(extern_function) => {
                    // All implicits of a libfunc are explicit implicits.
                    db.extern_function_declaration_implicits(extern_function)?.into_iter().collect()
                }
                GenericFunctionId::Trait(_) => unreachable!(),
            };
        all_implicits.extend(&current_implicits);
    }
    Ok(all_implicits)
}

/// Query implementation of [crate::db::LoweringGroup::function_with_body_all_implicits_vec].
pub fn function_with_body_all_implicits_vec(
    db: &dyn LoweringGroup,
    function: FunctionWithBodyId,
) -> Maybe<Vec<TypeId>> {
    let implicits_set = db.function_with_body_all_implicits(function)?;
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

/// Query implementation of [crate::db::LoweringGroup::function_with_body_scc].
pub fn function_with_body_scc(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Vec<FunctionWithBodyId> {
    compute_scc::<FunctionWithBodyNode<'_>>(FunctionWithBodyNode {
        function_with_body_id: function_id,
        db: db.upcast(),
    })
}

/// A node to use in the SCC computation.
#[derive(Clone)]
struct FunctionWithBodyNode<'a> {
    function_with_body_id: FunctionWithBodyId,
    db: &'a dyn LoweringGroup,
}
impl<'a> GraphNode for FunctionWithBodyNode<'a> {
    type NodeId = FunctionWithBodyId;

    fn get_neighbors(&self) -> Vec<Self> {
        self.db
            .function_with_body_direct_function_with_body_callees(self.function_with_body_id)
            .unwrap_or_default()
            .into_iter()
            .map(|function_with_body_id| FunctionWithBodyNode {
                function_with_body_id,
                db: self.db,
            })
            .collect()
    }

    fn get_id(&self) -> Self::NodeId {
        self.function_with_body_id
    }
}

/// Query implementation of [crate::db::LoweringGroup::function_may_panic].
pub fn function_may_panic(db: &dyn LoweringGroup, function: semantic::FunctionId) -> Maybe<bool> {
    match db.lookup_intern_function(function).function.generic_function {
        GenericFunctionId::Free(free_function) => {
            db.function_with_body_may_panic(FunctionWithBodyId::Free(free_function))
        }
        GenericFunctionId::Impl(impl_function) => {
            db.function_with_body_may_panic(FunctionWithBodyId::Impl(impl_function.function))
        }
        GenericFunctionId::Extern(extern_function) => {
            Ok(db.extern_function_signature(extern_function)?.panicable)
        }
        GenericFunctionId::Trait(_) => unreachable!(),
    }
}

/// Query implementation of [crate::db::LoweringGroup::function_with_body_may_panic].
pub fn function_with_body_may_panic(
    db: &dyn LoweringGroup,
    function: FunctionWithBodyId,
) -> Maybe<bool> {
    // Find the SCC representative.
    let scc_representative = db.function_scc_representative(function);

    // For each direct callee, find if it may panic.
    for direct_callee in db.function_with_body_direct_callees(function)? {
        // For a function with a body, call this method recursively. To avoid cycles, first
        // check that the callee is not in this function's SCC.
        let direct_callee_representative =
            match db.lookup_intern_function(direct_callee).function.generic_function {
                GenericFunctionId::Free(free_function) => {
                    function_scc_representative(db, FunctionWithBodyId::Free(free_function))
                }
                GenericFunctionId::Impl(impl_function) => function_scc_representative(
                    db,
                    FunctionWithBodyId::Impl(impl_function.function),
                ),
                GenericFunctionId::Extern(extern_function) => {
                    if db.extern_function_signature(extern_function)?.panicable {
                        return Ok(true);
                    }
                    continue;
                }
                GenericFunctionId::Trait(_) => unreachable!(),
            };
        if direct_callee_representative == scc_representative {
            // We already have the implicits of this SCC - do nothing.
            continue;
        }
        if db.function_with_body_may_panic(direct_callee_representative.0)? {
            return Ok(true);
        }
    }
    Ok(false)
}
