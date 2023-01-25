use cairo_lang_semantic::corelib::get_const_libfunc_name_by_type;
use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::extensions::lib_func::LibfuncSignature;
use cairo_lang_sierra::extensions::GenericLibfuncEx;
use cairo_lang_sierra::ids::{ConcreteLibfuncId, GenericLibfuncId};
use cairo_lang_sierra::program;
use itertools::Itertools;
use num_bigint::BigInt;
use semantic::items::functions::GenericFunctionId;
use smol_str::SmolStr;
use {cairo_lang_defs as defs, cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::specialization_context::SierraSignatureSpecializationContext;

pub fn simple_statement(
    libfunc_id: ConcreteLibfuncId,
    args: &[cairo_lang_sierra::ids::VarId],
    results: &[cairo_lang_sierra::ids::VarId],
) -> pre_sierra::Statement {
    pre_sierra::Statement::Sierra(program::GenStatement::Invocation(program::GenInvocation {
        libfunc_id,
        args: args.into(),
        branches: vec![program::GenBranchInfo {
            target: program::GenBranchTarget::Fallthrough,
            results: results.into(),
        }],
    }))
}

pub fn jump_statement(
    jump: ConcreteLibfuncId,
    label: pre_sierra::LabelId,
) -> pre_sierra::Statement {
    pre_sierra::Statement::Sierra(program::GenStatement::Invocation(program::GenInvocation {
        libfunc_id: jump,
        args: vec![],
        branches: vec![program::GenBranchInfo {
            target: program::GenBranchTarget::Statement(label),
            results: vec![],
        }],
    }))
}

pub fn return_statement(res: Vec<cairo_lang_sierra::ids::VarId>) -> pre_sierra::Statement {
    pre_sierra::Statement::Sierra(program::GenStatement::Return(res))
}

pub fn get_libfunc_id_with_generic_arg(
    db: &dyn SierraGenGroup,
    name: impl Into<SmolStr>,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string(name),
        generic_args: vec![cairo_lang_sierra::program::GenericArg::Type(ty)],
    })
}

/// Returns the [cairo_lang_sierra::program::ConcreteLibfuncLongId] associated with `store_temp`.
pub fn store_temp_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "store_temp", ty)
}

/// Returns the [cairo_lang_sierra::program::ConcreteLibfuncLongId] associated with `store_local`.
pub fn store_local_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "store_local", ty)
}

pub fn struct_construct_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "struct_construct", ty)
}

pub fn struct_deconstruct_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "struct_deconstruct", ty)
}

pub fn enum_init_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
    variant_idx: usize,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string("enum_init"),
        generic_args: vec![
            cairo_lang_sierra::program::GenericArg::Type(ty),
            cairo_lang_sierra::program::GenericArg::Value(variant_idx.into()),
        ],
    })
}

/// Returns the [cairo_lang_sierra::program::ConcreteLibfuncLongId] associated with `rename`.
pub fn rename_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string("rename"),
        generic_args: vec![cairo_lang_sierra::program::GenericArg::Type(ty)],
    })
}

fn get_libfunc_id_without_generics(
    db: &dyn SierraGenGroup,
    name: impl Into<SmolStr>,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string(name),
        generic_args: vec![],
    })
}

pub fn const_libfunc_id_by_type(
    db: &dyn SierraGenGroup,
    ty: semantic::TypeId,
    value: BigInt,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: cairo_lang_sierra::ids::GenericLibfuncId::from_string(
            get_const_libfunc_name_by_type(db.upcast(), ty),
        ),
        generic_args: vec![cairo_lang_sierra::program::GenericArg::Value(value)],
    })
}

pub fn match_enum_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "enum_match", ty)
}

pub fn drop_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "drop", ty)
}

pub fn dup_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "dup", ty)
}

pub fn branch_align_libfunc_id(
    db: &dyn SierraGenGroup,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "branch_align")
}

pub fn jump_libfunc_id(db: &dyn SierraGenGroup) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "jump")
}

pub fn revoke_ap_tracking_libfunc_id(
    db: &dyn SierraGenGroup,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "revoke_ap_tracking")
}

pub fn alloc_local_libfunc_id(
    db: &dyn SierraGenGroup,
    ty: cairo_lang_sierra::ids::ConcreteTypeId,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_with_generic_arg(db, "alloc_local", ty)
}

pub fn finalize_locals_libfunc_id(
    db: &dyn SierraGenGroup,
) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
    get_libfunc_id_without_generics(db, "finalize_locals")
}

/// Returns the [LibfuncSignature] of the given function.
pub fn get_libfunc_signature(
    db: &dyn SierraGenGroup,
    concrete_lib_func_id: ConcreteLibfuncId,
) -> LibfuncSignature {
    let libfunc_long_id = db.lookup_intern_concrete_lib_func(concrete_lib_func_id);
    // TODO(lior): replace expect() with a diagnostic (unless this can never happen).
    CoreLibfunc::specialize_signature_by_id(
        &SierraSignatureSpecializationContext(db),
        &libfunc_long_id.generic_id,
        &libfunc_long_id.generic_args,
    )
    .expect("Specialization failure.")
}

/// Returns the [ConcreteLibfuncId] for calling a user-defined function.
pub fn function_call_libfunc_id(
    db: &dyn SierraGenGroup,
    func: semantic::FunctionId,
) -> ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: GenericLibfuncId::from_string("function_call"),
        generic_args: vec![cairo_lang_sierra::program::GenericArg::UserFunc(
            db.intern_sierra_function(func),
        )],
    })
}

/// Returns the [ConcreteLibfuncId] used for calling a libfunc.
pub fn generic_libfunc_id(
    db: &dyn SierraGenGroup,
    extern_id: defs::ids::ExternFunctionId,
    generic_args: Vec<cairo_lang_sierra::program::GenericArg>,
) -> ConcreteLibfuncId {
    db.intern_concrete_lib_func(cairo_lang_sierra::program::ConcreteLibfuncLongId {
        generic_id: GenericLibfuncId::from_string(extern_id.name(db.upcast())),
        generic_args,
    })
}

/// Returns the [ConcreteLibfuncId] used for calling a function (either user-defined or libfunc).
pub fn get_concrete_libfunc_id(
    db: &dyn SierraGenGroup,
    function: semantic::FunctionId,
) -> (semantic::ConcreteFunction, ConcreteLibfuncId) {
    // Check if this is a user-defined function or a libfunc.
    let concrete_function = db.lookup_intern_function(function).function;
    match concrete_function.generic_function {
        GenericFunctionId::Free(_) | GenericFunctionId::Impl(_) => {
            (concrete_function, function_call_libfunc_id(db, function))
        }
        GenericFunctionId::Extern(extern_id) => {
            let mut generic_args = vec![];
            for generic_arg in &concrete_function.generic_args {
                generic_args.push(match generic_arg {
                    semantic::GenericArgumentId::Type(ty) => {
                        // TODO(lior): How should the following unwrap() be handled?
                        cairo_lang_sierra::program::GenericArg::Type(
                            db.get_concrete_type_id(*ty).unwrap(),
                        )
                    }
                    semantic::GenericArgumentId::Literal(literal_id) => {
                        cairo_lang_sierra::program::GenericArg::Value(
                            db.lookup_intern_literal(*literal_id).value,
                        )
                    }
                });
            }

            (concrete_function, generic_libfunc_id(db, extern_id, generic_args))
        }
        GenericFunctionId::Trait(_) => unreachable!(),
    }
}

/// Gets the output variables from a statement, including branching statements.
pub fn statement_outputs(
    statement: &lowering::Statement,
    lowered_function: &lowering::FlatLowered,
) -> Vec<lowering::VariableId> {
    match statement {
        lowering::Statement::CallBlock(statement_call_block) => {
            collect_outputs(lowered_function, &[statement_call_block.block])
        }
        lowering::Statement::MatchExtern(lowering::StatementMatchExtern { arms, .. })
        | lowering::Statement::MatchEnum(lowering::StatementMatchEnum { arms, .. }) => {
            let blocks = arms.iter().map(|(_, block)| *block).collect_vec();
            collect_outputs(lowered_function, &blocks)
        }
        _ => statement.outputs(),
    }
}

/// Collects output variables from multiple converging blocks.
fn collect_outputs(
    lowered_function: &lowering::FlatLowered,
    blocks: &[lowering::BlockId],
) -> Vec<id_arena::Id<lowering::Variable>> {
    for block in blocks {
        if let lowering::FlatBlockEnd::Callsite(remapping) = &lowered_function.blocks[*block].end {
            // It is guaranteed by lowering phase that all of the variables mapped to are the same.
            return remapping.keys().copied().collect();
        }
    }
    vec![]
}
