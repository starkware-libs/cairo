use cairo_lang_semantic::{corelib, GenericArgumentId};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use num_traits::Zero;

use crate::db::LoweringGroup;
use crate::ids::FunctionLongId;
use crate::{FlatLowered, Statement, StatementEnumConstruct, StatementLiteral, VarUsage, Variable};

/// Performs branch inversion optimization on a lowered function.
///
/// The branch inversion optimization finds a match enum whose input is the output of a call to
/// `bool_not_impl`.
/// It swaps the arms of the match enum and changes its input to be the input before the negation.
///
/// This optimization is valid only if all paths leading to the match enum pass through the call to
/// `bool_not_impl`. Therefore, the call to `bool_not_impl` should be in the same block as the match
/// enum.
///
/// The call to `bool_not_impl` is not deleted as we don't know if its output
/// is used by other statements (or block ending).
///
/// Due to the limitations above, the `reorder_statements` function should be called before this
/// optimization and between this optimization and the match optimization.
///
/// The first call to `reorder_statement`s moves the call to `bool_not_impl` into the block whose
/// match enum we want to optimize.
/// The second call to `reorder_statements` removes the call to `bool_not_impl` if it is unused,
/// allowing the match optimization to be applied to enum_init statements that appeared before the
/// `bool_not_impl`.
pub fn literal_propagation(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    let starknet_submodule = corelib::core_submodule(db.upcast(), "starknet");
    let storage_address_from_base_and_offset_func_id =
        db.intern_lowering_function(FunctionLongId::Semantic(corelib::get_function_id(
            db.upcast(),
            starknet_submodule,
            "storage_address_from_base_and_offset".into(),
            vec![],
        )));
    let storage_address_from_base_func_id =
        db.intern_lowering_function(FunctionLongId::Semantic(corelib::get_function_id(
            db.upcast(),
            starknet_submodule,
            "storage_address_from_base".into(),
            vec![],
        )));
    let u8_overflowing_add_func_id =
        db.intern_lowering_function(FunctionLongId::Semantic(corelib::get_function_id(
            db.upcast(),
            corelib::core_submodule(db.upcast(), "integer"),
            "u8_overflowing_add".into(),
            vec![],
        )));
    let u8_ty = corelib::get_core_ty_by_name(db.upcast(), "u8".into(), vec![]);
    for block in lowered.blocks.iter_mut() {
        let mut literals = UnorderedHashMap::default();
        let mut extra_inserts = vec![];
        for (i, stmt) in block.statements.iter_mut().enumerate() {
            match stmt {
                Statement::Literal(literal) => {
                    literals.insert(literal.output, literal.value.clone());
                }
                Statement::Call(call) => {
                    if call.function == storage_address_from_base_and_offset_func_id
                        && literals
                            .get(&call.inputs[1].var_id)
                            .map(|value| value.is_zero())
                            .unwrap_or_default()
                    {
                        call.function = storage_address_from_base_func_id;
                        call.inputs = vec![call.inputs[0]];
                    } else if call.function == u8_overflowing_add_func_id {
                        if let (Some(lhs), Some(rhs)) = (
                            literals.get(&call.inputs[0].var_id),
                            literals.get(&call.inputs[1].var_id),
                        ) {
                            if let Some(value) = lhs.checked_add(rhs) {
                                let other_var = &lowered.variables[call.inputs[0].var_id];
                                let literal_var = lowered.variables.alloc(Variable {
                                    location: call.location,
                                    ..other_var.clone()
                                });
                                extra_inserts.push((
                                    i,
                                    Statement::Literal(StatementLiteral {
                                        output: literal_var,
                                        value,
                                    }),
                                ));
                                *stmt = Statement::EnumConstruct(StatementEnumConstruct {
                                    variant: corelib::option_some_variant(
                                        db.upcast(),
                                        GenericArgumentId::Type(u8_ty),
                                    ),
                                    input: VarUsage {
                                        var_id: literal_var,
                                        location: call.location,
                                    },
                                    output: call.outputs[0],
                                });
                            }
                        }
                    }
                }
                Statement::EnumConstruct(_)
                | Statement::StructConstruct(_)
                | Statement::StructDestructure(_)
                | Statement::Snapshot(_)
                | Statement::Desnap(_) => {}
            }
        }
        for (inserted, (i, extra_insert)) in extra_inserts.into_iter().enumerate() {
            block.statements.insert(i + inserted, extra_insert);
        }
    }
}
