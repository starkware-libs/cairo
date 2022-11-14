use std::collections::HashMap;
use std::vec;

use casm::ap_change::ApChange;
use casm::casm;
use casm::instructions::Instruction;
use itertools::{zip_eq, Itertools};
use pretty_assertions::assert_eq;
use sierra::extensions::core::{CoreLibFunc, CoreType};
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
use crate::invocations::compile_invocation;
use crate::metadata::Metadata;
use crate::ref_expr;
use crate::references::{ReferenceExpression, ReferenceValue};
use crate::relocations::RelocationEntry;

/// Specialization context for libfuncs and types, based on string names only, allows building
/// libfuncs without salsa db or program registry.
struct MockSpecializationContext {}
impl TypeSpecializationContext for MockSpecializationContext {
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
impl SignatureSpecializationContext for MockSpecializationContext {
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
impl SpecializationContext for MockSpecializationContext {
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

/// Information from [BranchChanges] we should test when only testing a libfunc lowering by itself.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReducedBranchChanges {
    /// New references defined at a given branch.
    /// should correspond to BranchInfo.results.
    pub refs: Vec<ReferenceExpression>,
    /// The change to AP caused by the libfunc in the branch.
    pub ap_change: ApChange,
}

/// Information from [CompiledInvocation] we should test when only testing a libfunc lowering by
/// itself.
#[derive(Debug, Eq, PartialEq)]
struct ReducedCompiledInvocation {
    /// A vector of instructions that implement the invocation.
    pub instructions: Vec<Instruction>,
    /// A vector of static relocations.
    pub relocations: Vec<RelocationEntry>,
    /// A vector of ReducedBranchChanges, should correspond to the branches of the invocation
    /// statement.
    pub results: Vec<ReducedBranchChanges>,
}
impl ReducedCompiledInvocation {
    fn new(compiled_invocation: CompiledInvocation) -> ReducedCompiledInvocation {
        ReducedCompiledInvocation {
            instructions: compiled_invocation.instructions,
            relocations: compiled_invocation.relocations,
            results: compiled_invocation
                .results
                .into_iter()
                .map(|result| ReducedBranchChanges {
                    refs: result.refs.into_iter().map(|r| r.expression).collect(),
                    ap_change: result.ap_change,
                })
                .collect(),
        }
    }
}

// Compiles the last libfunc invocation in sierra_code.
fn compile_libfunc(libfunc: &str, refs: Vec<ReferenceExpression>) -> ReducedCompiledInvocation {
    let long_id =
        sierra::ConcreteLibFuncLongIdParser::new().parse(libfunc.to_string().as_str()).unwrap();
    let context = MockSpecializationContext {};
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
    ReducedCompiledInvocation::new(
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
        .expect("Failed to compile invocation."),
    )
}

#[test]
fn test_felt_add() {
    assert_eq!(
        compile_libfunc("felt_add", vec![ref_expr!([fp + 5]), ref_expr!([ap + 5])]),
        ReducedCompiledInvocation {
            instructions: vec![],
            relocations: vec![],
            results: vec![ReducedBranchChanges {
                refs: vec![ref_expr!([fp + 5] + [ap + 5])],
                ap_change: ApChange::Known(0)
            }]
        }
    );
}

#[test]
fn test_store_temp() {
    assert_eq!(
        compile_libfunc("store_temp<felt>", vec![ref_expr!([fp + 5] + [ap + 5])]),
        ReducedCompiledInvocation {
            instructions: casm! {[ap + 0] = [fp + 5] + [ap + 5], ap++;}.instructions,
            relocations: vec![],
            results: vec![ReducedBranchChanges {
                refs: vec![ref_expr!([ap - 1])],
                ap_change: ApChange::Known(1)
            }]
        }
    );
}
