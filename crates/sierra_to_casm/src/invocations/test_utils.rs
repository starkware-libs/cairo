use std::collections::HashMap;

use casm::ap_change::ApChange;
use casm::instructions::Instruction;
use itertools::{zip_eq, Itertools};
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::extensions::lib_func::{SignatureSpecializationContext, SpecializationContext};
use sierra::extensions::type_specialization_context::TypeSpecializationContext;
use sierra::extensions::types::TypeInfo;
use sierra::extensions::{ConcreteLibFunc, ConcreteType, GenericLibFuncEx, GenericTypeEx};
use sierra::ids::{ConcreteTypeId, VarId};
use sierra::program::{BranchInfo, BranchTarget, Invocation, StatementIdx};
use sierra_ap_change::ap_change_info::ApChangeInfo;
use sierra_gas::gas_info::GasInfo;

use super::{compile_invocation, CompiledInvocation, ProgramInfo};
use crate::environment::gas_wallet::GasWallet;
use crate::environment::Environment;
use crate::metadata::Metadata;
use crate::references::{ReferenceExpression, ReferenceValue};
use crate::relocations::RelocationEntry;

/// Creates a FeltBinaryOperator from a token operator.
#[macro_export]
macro_rules! cell_expr_operator {
    (+) => {
        sierra::extensions::felt::FeltBinaryOperator::Add
    };
    (-) => {
        sierra::extensions::felt::FeltBinaryOperator::Sub
    };
    (*) => {
        sierra::extensions::felt::FeltBinaryOperator::Mul
    };
    (/) => {
        sierra::extensions::felt::FeltBinaryOperator::Div
    };
}

/// Adds a cell expression to a ReferenceExpression cells vector.
#[macro_export]
macro_rules! ref_expr_extend {
    ($cells:ident) => {};
    ($cells:ident, [$a:ident $($op:tt $offset:expr)?] $operator:tt $b:tt $(, $tok:tt)*) => {
        $cells.push(
            $crate::references::CellExpression::BinOp($crate::references::BinOpExpression {
                op: $crate::cell_expr_operator!($operator),
                a: casm::deref!([$a $($op $offset)?]),
                b: casm::deref_or_immediate!($b),
        }));
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
    ($cells:ident, [$a:ident $($op:tt $offset:expr)?] $(, $tok:tt)*) => {
        $cells.push(
            $crate::references::CellExpression::Deref(casm::deref!([$a $($op $offset)?]))
        );
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
    ($cells:ident, [[$a:ident $($op:tt $offset:expr)?]] $(, $tok:tt)*) => {
        $cells.push(
            $crate::references::CellExpression::DoubleDeref(casm::deref!([$a $($op $offset)?]), 0)
        );
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
    ($cells:ident, [[$a:ident $($op:tt $offset:expr)?] + $offset2:expr] $(, $tok:tt)*) => {
        $cells.push(
            $crate::references::CellExpression::DoubleDeref(casm::deref!([$a $($op $offset)?]), $offset2)
        );
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
    ($cells:ident, & $a:ident $($op:tt $offset:expr)? $(, $tok:tt)*) => {
        $cells.push($crate::references::CellExpression::IntoSingleCellRef(
            casm::deref!([$a $($op $offset)?])
        ));
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
    ($cells:ident, $a:expr $(, $tok:tt)*) => {
        cells.push(
            $crate::references::CellExpression::Immediate(num_bigint::BigInt::from($a))
        );
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
    ($cells:ident, _ $(, $tok:tt)*) => {
        cells.push($crate::references::CellExpression::Padding);
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
}

// TODO(orizi): Make this flexible enough to be used in outside of testing.
/// Creates a reference expression.
#[macro_export]
macro_rules! ref_expr {
    {$($tok:tt)*} => {
        {
            let mut cells = Vec::new();
            #[allow(clippy::vec_init_then_push)]
            {
                $crate::ref_expr_extend!(cells, $($tok)*);
            }
            $crate::references::ReferenceExpression { cells }
        }
    }
}

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
#[derive(Eq, PartialEq)]
pub struct ReducedCompiledInvocation {
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
impl std::fmt::Debug for ReducedCompiledInvocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReducedCompiledInvocation")
            .field(
                "instructions",
                &self.instructions.iter().map(|inst| format!("{inst};")).collect_vec(),
            )
            .field("relocations", &self.relocations)
            .field("results", &self.results)
            .finish()
    }
}

/// Compiles a libfunc into a [ReducedCompiledInvocation].
/// the arguments are auto-filled according to the signature
/// I.e. the libfunc is invoked by:
/// libfunc(*refs) -> {
///     0([0], [2],..., [n_0])
///     1([0], [2],..., [n_1])
///     ...
///     Fallthrough([0], [2],..., [n_fallthrough_idx])
///     ...
///     k([0], [2],..., [n_k])
/// }
pub fn compile_libfunc(libfunc: &str, refs: Vec<ReferenceExpression>) -> ReducedCompiledInvocation {
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
            ap_change_info: ApChangeInfo {
                variable_values: HashMap::default(),
                function_ap_change: HashMap::default(),
            },
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
                branches: libfunc
                    .branch_signatures()
                    .iter()
                    .enumerate()
                    .map(|(i, branch)| BranchInfo {
                        target: if libfunc.fallthrough() == Some(i) {
                            BranchTarget::Fallthrough
                        } else {
                            BranchTarget::Statement(StatementIdx(i))
                        },
                        results: (0..branch.vars.len()).map(VarId::from_usize).collect(),
                    })
                    .collect(),
            },
            &libfunc,
            StatementIdx(0),
            &args,
            environment,
        )
        .expect("Failed to compile invocation."),
    )
}
