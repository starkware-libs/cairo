use std::vec;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::{GenericArgumentId, TypeId};
use cairo_lang_utils::LookupIntern;
use itertools::{Itertools, chain, zip_eq};

use crate::blocks::BlocksBuilder;
use crate::db::LoweringGroup;
use crate::ids::{self, LocationId, SemanticFunctionIdEx, SpecializedFunction};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::{
    Block, BlockEnd, DependencyType, Lowered, LoweringStage, Statement, StatementCall,
    StatementConst, VarUsage, VariableId,
};

// A const argument for a specialized function.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SpecializationArg {
    Const(ConstValue),
    EmptyArray(TypeId),
}

impl<'a> DebugWithDb<dyn LoweringGroup + 'a> for SpecializationArg {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn LoweringGroup + 'a),
    ) -> std::fmt::Result {
        match self {
            SpecializationArg::Const(value) => write!(f, "{:?}", value.debug(db)),
            SpecializationArg::EmptyArray(_) => write!(f, "array![]"),
        }
    }
}

/// Returns the lowering of a specialized function.
pub fn specialized_function_lowered(
    db: &dyn LoweringGroup,
    specialized: SpecializedFunction,
) -> Maybe<Lowered> {
    let base = db.lowered_body(specialized.base, LoweringStage::Monomorphized)?;
    let base_semantic = specialized.base.base_semantic_function(db);

    let array_new_fn = GenericFunctionId::Extern(
        ModuleHelper::core(db).submodule("array").extern_function_id("array_new"),
    );

    let mut variables =
        VariableAllocator::new(db, base_semantic.function_with_body_id(db), Default::default())?;
    let mut statements = vec![];
    let mut parameters = vec![];

    for (param, arg) in zip_eq(&base.parameters, specialized.args.iter()) {
        let var_id = variables.variables.alloc(base.variables[*param].clone());
        if let Some(arg) = arg {
            match arg {
                SpecializationArg::Const(value) => {
                    statements.push(Statement::Const(StatementConst {
                        value: value.clone(),
                        output: var_id,
                    }));
                }
                SpecializationArg::EmptyArray(ty) => {
                    statements.push(Statement::Call(StatementCall {
                        function: array_new_fn
                            .concretize(db, vec![GenericArgumentId::Type(*ty)])
                            .lowered(db),
                        inputs: vec![],
                        with_coupon: false,
                        outputs: vec![var_id],
                        location: variables[var_id].location,
                    }));
                }
            }

            continue;
        }
        parameters.push(var_id);
    }
    let location = LocationId::from_stable_location(
        db,
        specialized.base.base_semantic_function(db).stable_location(db),
    );
    let inputs =
        variables.variables.iter().map(|(var_id, _)| VarUsage { var_id, location }).collect();
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
pub fn priv_should_specialize(
    db: &dyn LoweringGroup,
    function_id: ids::ConcreteFunctionWithBodyId,
) -> Maybe<bool> {
    let ids::ConcreteFunctionWithBodyLongId::Specialized(specialized_func) =
        function_id.lookup_intern(db)
    else {
        panic!("Expected a specialized function");
    };

    // Breaks cycles.
    // We cannot estimate the size of functions in a cycle, since the implicits computation requires
    // the finalized lowering of all the functions in the cycle which requires us to know the
    // answer of the current function.
    if db.concrete_in_cycle(
        specialized_func.base,
        DependencyType::Call,
        LoweringStage::Monomorphized,
    )? {
        return Ok(false);
    }

    // The heuristic is that the size is 8/10*orig_size > specialized_size of the original size.
    Ok(8 * db.estimate_size(specialized_func.base)? > 10 * db.estimate_size(function_id)?)
}
