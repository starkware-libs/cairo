use std::collections::HashMap;
use std::vec;

use casm::ap_change::ApChange;
use casm::operand::DerefOrImmediate;
use casm::{casm, deref};
use itertools::{zip_eq, Itertools};
use pretty_assertions::assert_eq;
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::extensions::felt::FeltOperator;
use sierra::extensions::lib_func::{SignatureSpecializationContext, SpecializationContext};
use sierra::extensions::type_specialization_context::TypeSpecializationContext;
use sierra::extensions::types::TypeInfo;
use sierra::extensions::{ConcreteLibFunc, ConcreteType, GenericLibFuncEx, GenericTypeEx};
use sierra::ids::{ConcreteTypeId, VarId};
use sierra::program::{Invocation, StatementIdx};
use sierra_gas::gas_info::GasInfo;

use super::{CompiledInvocation, ProgramInfo};
use crate::environment::gas_wallet::GasWallet;
use crate::environment::Environment;
use crate::invocations::{compile_invocation, BranchChanges};
use crate::metadata::Metadata;
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue};

fn as_ref_expr(cell_expr: CellExpression) -> ReferenceExpression {
    ReferenceExpression::from_cell(cell_expr)
}

struct TestSpecializationContext {}
impl TypeSpecializationContext for TestSpecializationContext {
    fn try_get_type_info(&self, id: ConcreteTypeId) -> Option<TypeInfo> {
        let long_id =
            sierra::ConcreteTypeLongIdParser::new().parse(id.to_string().as_str()).unwrap();
        Some(
            CoreType::specialize_by_id(self, &long_id.generic_id, &long_id.generic_args)
                .ok()?
                .info()
                .clone(),
        )
    }
}
impl SignatureSpecializationContext for TestSpecializationContext {
    fn try_get_concrete_type(
        &self,
        id: sierra::ids::GenericTypeId,
        generic_args: &[sierra::program::GenericArg],
    ) -> Option<ConcreteTypeId> {
        Some(if generic_args.is_empty() {
            id.to_string().into()
        } else {
            format!(
                "{id}<{}>",
                generic_args.iter().map(sierra::program::GenericArg::to_string).join(", ")
            )
            .into()
        })
    }

    fn try_get_function_signature(
        &self,
        _function_id: &sierra::ids::FunctionId,
    ) -> Option<sierra::program::FunctionSignature> {
        unreachable!("Function related specialization functionalities are not implemented.")
    }

    fn try_get_function_ap_change(
        &self,
        _function_id: &sierra::ids::FunctionId,
    ) -> Option<sierra::extensions::lib_func::SierraApChange> {
        unreachable!("Function related specialization functionalities are not implemented.")
    }

    fn as_type_specialization_context(&self) -> &dyn TypeSpecializationContext {
        self
    }
}
impl SpecializationContext for TestSpecializationContext {
    fn upcast(&self) -> &dyn SignatureSpecializationContext {
        self
    }

    fn try_get_function(
        &self,
        _function_id: &sierra::ids::FunctionId,
    ) -> Option<sierra::program::Function> {
        unreachable!("Function related specialization functionalities are not implemented.")
    }
}

// Compiles the last libfunc invocation in sierra_code.
fn compile_libfunc(libfunc: &str, refs: Vec<ReferenceExpression>) -> CompiledInvocation {
    let long_id =
        sierra::ConcreteLibFuncLongIdParser::new().parse(libfunc.to_string().as_str()).unwrap();
    let context = TestSpecializationContext {};
    let libfunc =
        CoreLibFunc::specialize_by_id(&context, &long_id.generic_id, &long_id.generic_args)
            .unwrap();

    let mut type_sizes = HashMap::default();
    for param in libfunc.param_signatures() {
        type_sizes
            .insert(param.ty.clone(), context.try_get_type_info(param.ty.clone()).unwrap().size);
    }
    for branch_signature in libfunc.branch_signatures() {
        for var in &branch_signature.vars {
            type_sizes
                .insert(var.ty.clone(), context.try_get_type_info(var.ty.clone()).unwrap().size);
        }
    }
    let program_info = ProgramInfo {
        metadata: &Metadata {
            function_ap_change: HashMap::new(),
            gas_info: GasInfo { variable_values: HashMap::new(), function_costs: HashMap::new() },
        },
        type_sizes: &type_sizes,
    };

    let args: Vec<ReferenceValue> = zip_eq(refs.into_iter(), libfunc.param_signatures())
        .map(|(expression, param)| ReferenceValue { expression, ty: param.ty.clone() })
        .collect();

    let environment = Environment::new(GasWallet::Disabled);
    compile_invocation(
        program_info,
        &Invocation {
            libfunc_id: "".into(),
            args: (0..args.len()).map(VarId::from_usize).collect(),
            branches: vec![],
        },
        &libfunc,
        StatementIdx(0),
        &args,
        environment,
    )
    .expect("Failed to compile invocation.")
}

#[test]
fn test_felt_add() {
    let refs: Vec<ReferenceExpression> = [deref!([fp + 5]), deref!([ap + 5])]
        .map(|x| as_ref_expr(CellExpression::Deref(x)))
        .into_iter()
        .collect();
    let compiled_invocation = compile_libfunc("felt_add", refs);

    let expected_ref = as_ref_expr(CellExpression::BinOp(BinOpExpression {
        op: FeltOperator::Add,
        a: deref!([fp + 5]),
        b: DerefOrImmediate::Deref(deref!([ap + 5])),
    }));
    let felt_ty = ConcreteTypeId::from_string("felt");
    let expected = CompiledInvocation {
        instructions: vec![],
        relocations: vec![],
        results: vec![BranchChanges {
            refs: vec![ReferenceValue { expression: expected_ref, ty: felt_ty }],
            ap_change: ApChange::Known(0),
            gas_change: 0,
        }],
        environment: Environment::new(GasWallet::Disabled),
    };
    assert_eq!(compiled_invocation, expected);
}

#[test]
fn test_store_temp() {
    let refs = vec![as_ref_expr(CellExpression::BinOp(BinOpExpression {
        op: FeltOperator::Add,
        a: deref!([fp + 5]),
        b: DerefOrImmediate::Deref(deref!([ap + 5])),
    }))];
    let compiled_invocation = compile_libfunc("store_temp<felt>", refs);

    let expected_ref = as_ref_expr(CellExpression::Deref(deref!([ap + -1])));
    let felt_ty = ConcreteTypeId::from_string("felt");
    let expected = CompiledInvocation {
        instructions: casm! {[ap + 0] = [fp + 5] + [ap + 5], ap++;}.instructions,
        relocations: vec![],
        results: vec![BranchChanges {
            refs: vec![ReferenceValue { expression: expected_ref, ty: felt_ty }],
            ap_change: ApChange::Known(1),
            gas_change: -1,
        }],
        environment: Environment::new(GasWallet::Disabled),
    };
    assert_eq!(compiled_invocation, expected);
}
