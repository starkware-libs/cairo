use std::vec;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::items::constant::ConstValueId;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_semantic::{ConcreteTypeId, ConcreteVariant, GenericArgumentId, TypeId, TypeLongId};
use cairo_lang_utils::extract_matches;
use itertools::{Itertools, chain, zip_eq};
use salsa::Database;

use crate::blocks::BlocksBuilder;
use crate::db::LoweringGroup;
use crate::ids::{self, LocationId, SemanticFunctionIdEx, SpecializedFunction};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::objects::StatementEnumConstruct as StatementEnumConstructObj;
use crate::{
    Block, BlockEnd, Lowered, LoweringStage, Statement, StatementCall, StatementConst,
    StatementSnapshot, StatementStructConstruct, VarUsage, VariableId,
};

// A const argument for a specialized function.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SpecializationArg<'db> {
    Const { value: ConstValueId<'db>, boxed: bool },
    Snapshot(Box<SpecializationArg<'db>>),
    Array(TypeId<'db>, Vec<SpecializationArg<'db>>),
    Struct(Vec<SpecializationArg<'db>>),
    Enum { variant: ConcreteVariant<'db>, payload: Box<SpecializationArg<'db>> },
    NotSpecialized,
}

impl<'a> DebugWithDb<'a> for SpecializationArg<'a> {
    type Db = dyn Database;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'a dyn Database) -> std::fmt::Result {
        match self {
            SpecializationArg::Const { value, boxed } => {
                write!(f, "{:?}", value.debug(db))?;
                if *boxed {
                    write!(f, ".into_box()")?;
                }
                Ok(())
            }
            SpecializationArg::Snapshot(inner) => write!(f, "@{:?}", inner.debug(db)),
            SpecializationArg::Struct(args) => {
                write!(f, "{{")?;
                let mut inner = args.iter().peekable();
                while let Some(value) = inner.next() {
                    write!(f, " ")?;
                    value.fmt(f, db)?;

                    if inner.peek().is_some() {
                        write!(f, ",")?;
                    } else {
                        write!(f, " ")?;
                    }
                }
                write!(f, "}}")
            }
            SpecializationArg::Array(_ty, values) => {
                write!(f, "array![")?;
                let mut first = true;
                for value in values {
                    if !first {
                        write!(f, ", ")?;
                    } else {
                        first = false;
                    }
                    write!(f, "{:?}", value.debug(db))?;
                }
                write!(f, "]")
            }
            SpecializationArg::Enum { variant, payload } => {
                write!(f, "{:?}(", variant.debug(db))?;
                payload.fmt(f, db)?;
                write!(f, ")")
            }
            SpecializationArg::NotSpecialized => write!(f, "NotSpecialized"),
        }
    }
}

/// The state of the specialization arg building process.
/// currently only structs require an additional build step.
enum SpecializationArgBuildingState<'db, 'a> {
    Initial(&'a SpecializationArg<'db>),
    TakeSnapshot(VariableId),
    BuildStruct(Vec<VariableId>),
    PushBackArray { in_array: VariableId, value: VariableId },
    BuildEnum { variant: ConcreteVariant<'db>, payload: VariableId },
}

/// Returns the lowering of a specialized function.
pub fn specialized_function_lowered<'db>(
    db: &'db dyn Database,
    specialized: SpecializedFunction<'db>,
) -> Maybe<Lowered<'db>> {
    let base = db.lowered_body(specialized.base, LoweringStage::Monomorphized)?;
    let base_semantic = specialized.base.base_semantic_function(db);

    let array_module = ModuleHelper::core(db).submodule("array");
    let array_new_fn = GenericFunctionId::Extern(array_module.extern_function_id("array_new"));
    let array_append = GenericFunctionId::Extern(array_module.extern_function_id("array_append"));

    let mut variables =
        VariableAllocator::new(db, base_semantic.function_with_body_id(db), Default::default())?;
    let mut statements = vec![];
    let mut parameters = vec![];
    let mut inputs = vec![];
    let mut stack = vec![];

    let location = LocationId::from_stable_location(
        db,
        specialized.base.base_semantic_function(db).stable_location(db),
    );

    for (param, arg) in zip_eq(&base.parameters, specialized.args.iter()) {
        let var_id = variables.variables.alloc(base.variables[*param].clone());
        inputs.push(VarUsage { var_id, location });
        if SpecializationArg::NotSpecialized == *arg {
            parameters.push(var_id);
            continue;
        }
        stack.push((var_id, SpecializationArgBuildingState::Initial(arg)));
        while let Some((var_id, state)) = stack.pop() {
            match state {
                SpecializationArgBuildingState::Initial(c) => match c {
                    SpecializationArg::Const { value, boxed } => {
                        statements
                            .push(Statement::Const(StatementConst::new(*value, var_id, *boxed)));
                    }
                    SpecializationArg::Snapshot(inner) => {
                        let snap_ty = variables.variables[var_id].ty;
                        let denapped_ty = *extract_matches!(snap_ty.long(db), TypeLongId::Snapshot);
                        let desnapped_var =
                            variables.new_var(VarRequest { ty: denapped_ty, location });
                        stack.push((
                            var_id,
                            SpecializationArgBuildingState::TakeSnapshot(desnapped_var),
                        ));
                        stack.push((
                            desnapped_var,
                            SpecializationArgBuildingState::Initial(inner.as_ref()),
                        ));
                    }
                    SpecializationArg::Array(ty, values) => {
                        let mut arr_var = var_id;
                        for value in values.iter().rev() {
                            let in_arr_var =
                                variables.variables.alloc(variables.variables[var_id].clone());
                            let value_var = variables.new_var(VarRequest { ty: *ty, location });
                            stack.push((
                                arr_var,
                                SpecializationArgBuildingState::PushBackArray {
                                    in_array: in_arr_var,
                                    value: value_var,
                                },
                            ));
                            stack.push((value_var, SpecializationArgBuildingState::Initial(value)));
                            arr_var = in_arr_var;
                        }
                        statements.push(Statement::Call(StatementCall {
                            function: array_new_fn
                                .concretize(db, vec![GenericArgumentId::Type(*ty)])
                                .lowered(db),
                            inputs: vec![],
                            with_coupon: false,
                            outputs: vec![arr_var],
                            location: variables[var_id].location,
                            is_specialization_base_call: false,
                        }));
                    }
                    SpecializationArg::Struct(args) => {
                        let var = &variables[var_id];
                        let TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct)) =
                            var.ty.long(db)
                        else {
                            unreachable!("Expected a concrete struct type");
                        };

                        let members = db.concrete_struct_members(*concrete_struct)?;

                        let location = var.location;
                        let var_ids = members
                            .values()
                            .map(|member| variables.new_var(VarRequest { ty: member.ty, location }))
                            .collect_vec();

                        stack.push((
                            var_id,
                            SpecializationArgBuildingState::BuildStruct(var_ids.clone()),
                        ));

                        for (var_id, arg) in zip_eq(var_ids.iter().rev(), args.iter().rev()) {
                            stack.push((*var_id, SpecializationArgBuildingState::Initial(arg)));
                        }
                    }
                    SpecializationArg::Enum { variant, payload } => {
                        let location = variables[var_id].location;
                        let payload_var =
                            variables.new_var(VarRequest { ty: variant.ty, location });
                        stack.push((
                            var_id,
                            SpecializationArgBuildingState::BuildEnum {
                                variant: *variant,
                                payload: payload_var,
                            },
                        ));
                        stack.push((
                            payload_var,
                            SpecializationArgBuildingState::Initial(payload.as_ref()),
                        ));
                    }
                    SpecializationArg::NotSpecialized => {
                        parameters.push(var_id);
                    }
                },
                SpecializationArgBuildingState::TakeSnapshot(desnapped_var) => {
                    let ignored = variables.variables.alloc(variables[desnapped_var].clone());
                    statements.push(Statement::Snapshot(StatementSnapshot::new(
                        VarUsage { var_id: desnapped_var, location },
                        ignored,
                        var_id,
                    )));
                }
                SpecializationArgBuildingState::PushBackArray { in_array, value } => {
                    statements.push(Statement::Call(StatementCall {
                        function: array_append
                            .concretize(
                                db,
                                vec![GenericArgumentId::Type(variables.variables[value].ty)],
                            )
                            .lowered(db),
                        inputs: vec![
                            VarUsage { var_id: in_array, location },
                            VarUsage { var_id: value, location },
                        ],
                        with_coupon: false,
                        outputs: vec![var_id],
                        location,
                        is_specialization_base_call: false,
                    }));
                }
                SpecializationArgBuildingState::BuildStruct(ids) => {
                    statements.push(Statement::StructConstruct(StatementStructConstruct {
                        inputs: ids
                            .iter()
                            .map(|id| VarUsage { var_id: *id, location: variables[*id].location })
                            .collect(),
                        output: var_id,
                    }));
                }
                SpecializationArgBuildingState::BuildEnum { variant, payload } => {
                    statements.push(Statement::EnumConstruct(StatementEnumConstructObj {
                        variant,
                        input: VarUsage { var_id: payload, location: variables[payload].location },
                        output: var_id,
                    }));
                }
            }
        }
    }

    let outputs: Vec<VariableId> =
        chain!(base.signature.extra_rets.iter().map(|ret| ret.ty()), [base.signature.return_type])
            .map(|ty| variables.new_var(VarRequest { ty, location }))
            .collect_vec();
    let mut block_builder = BlocksBuilder::new();
    let ret_usage =
        outputs.iter().map(|var_id| VarUsage { var_id: *var_id, location }).collect_vec();
    statements.push(Statement::Call(StatementCall {
        function: specialized.base.function_id(db)?,
        with_coupon: false,
        inputs,
        outputs,
        location,
        is_specialization_base_call: true,
    }));
    block_builder.alloc(Block { statements, end: BlockEnd::Return(ret_usage, location) });
    Ok(Lowered {
        signature: specialized.signature(db)?,
        variables: variables.variables,
        blocks: block_builder.build().unwrap(),
        parameters,
        diagnostics: Default::default(),
    })
}

/// Query implementation of [LoweringGroup::priv_should_specialize].
#[salsa::tracked]
pub fn priv_should_specialize<'db>(
    db: &'db dyn Database,
    function_id: ids::ConcreteFunctionWithBodyId<'db>,
) -> Maybe<bool> {
    let ids::ConcreteFunctionWithBodyLongId::Specialized(SpecializedFunction { base, .. }) =
        function_id.long(db)
    else {
        panic!("Expected a specialized function");
    };

    // The heuristic is that the size is 8/10*orig_size > specialized_size of the original size.
    Ok(db.estimate_size(*base)?.saturating_mul(8)
        > db.estimate_size(function_id)?.saturating_mul(10))
}
