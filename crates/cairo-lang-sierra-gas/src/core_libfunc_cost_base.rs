use cairo_lang_sierra::extensions::array::ArrayConcreteLibfunc;
use cairo_lang_sierra::extensions::boolean::BoolConcreteLibfunc;
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::extensions::builtin_cost::{
    BuiltinCostConcreteLibfunc, BuiltinCostGetGasLibfunc, CostTokenType,
};
use cairo_lang_sierra::extensions::core::CoreConcreteLibfunc::{
    self, ApTracking, Array, Bitwise, Bool, Box, BranchAlign, BuiltinCost, DictFeltTo, Drop, Dup,
    Ec, Enum, Felt, FunctionCall, Gas, Mem, Pedersen, Struct, Uint128, Uint64, Uint8,
    UnconditionalJump, UnwrapNonZero,
};
use cairo_lang_sierra::extensions::dict_felt_to::DictFeltToConcreteLibfunc;
use cairo_lang_sierra::extensions::ec::EcConcreteLibfunc;
use cairo_lang_sierra::extensions::enm::EnumConcreteLibfunc;
use cairo_lang_sierra::extensions::felt::FeltConcrete;
use cairo_lang_sierra::extensions::function_call::FunctionCallConcreteLibfunc;
use cairo_lang_sierra::extensions::gas::GasConcreteLibfunc::{GetGas, RefundGas};
use cairo_lang_sierra::extensions::mem::MemConcreteLibfunc::{
    AlignTemps, AllocLocal, FinalizeLocals, Rename, StoreLocal, StoreTemp,
};
use cairo_lang_sierra::extensions::nullable::NullableConcreteLibfunc;
use cairo_lang_sierra::extensions::structure::StructConcreteLibfunc;
use cairo_lang_sierra::extensions::uint::{IntOperator, Uint64Concrete, Uint8Concrete};
use cairo_lang_sierra::extensions::uint128::Uint128Concrete;
use cairo_lang_sierra::extensions::ConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::Function;
use itertools::Itertools;

use crate::starknet_libfunc_cost_base::starknet_libfunc_cost_base;

#[derive(Default, Debug)]
pub struct ConstCost {
    pub steps: i32,
    pub holes: i32,
    pub range_checks: i32,
}
impl ConstCost {
    pub const fn cost(&self) -> i32 {
        self.steps * 100 + self.holes * 10 + self.range_checks * 70
    }
}

/// The cost of a dictionary access in the squash stage.
pub const DICT_SQUASH_ACCESS_COST: i32 = ConstCost { steps: 84, holes: 0, range_checks: 0 }.cost();

/// The operation required for extracting a libfunc's cost.
pub trait CostOperations {
    type CostType: Clone;

    /// Gets a cost from a constant value (of type [CostTokenType::Const]).
    fn const_cost(&self, value: ConstCost) -> Self::CostType {
        self.cost_token(value.cost(), CostTokenType::Const)
    }

    /// Gets a cost from step count.
    fn steps(&self, steps: i32) -> Self::CostType {
        self.const_cost(ConstCost { steps, ..ConstCost::default() })
    }

    /// Gets a cost from hole count.
    fn holes(&self, holes: i32) -> Self::CostType {
        self.const_cost(ConstCost { holes, ..ConstCost::default() })
    }

    /// Gets a cost from range check count.
    fn range_checks(&self, range_checks: i32) -> Self::CostType {
        self.const_cost(ConstCost { range_checks, ..ConstCost::default() })
    }

    /// Gets a cost of the given token type.
    fn cost_token(&self, count: i32, token_type: CostTokenType) -> Self::CostType;
    /// Gets a cost for the content of a function.
    fn function_token_cost(
        &mut self,
        function: &Function,
        token_type: CostTokenType,
    ) -> Self::CostType;
    /// Gets a cost for a variable for the current statement.
    fn statement_var_cost(&self, token_type: CostTokenType) -> Self::CostType;
    /// Adds costs.
    fn add(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType;
    /// Subtracts costs.
    fn sub(&self, lhs: Self::CostType, rhs: Self::CostType) -> Self::CostType;
}

/// Trait for providing extra information required for calculating costs for a specific libfunc
/// invocation.
pub trait InvocationCostInfoProvider {
    /// Provides the sizes of types.
    fn type_size(&self, ty: &ConcreteTypeId) -> usize;
    /// Number of tokens provided by the libfunc invocation (currently only relevant for
    /// `get_gas_all`).
    fn token_usages(&self, token_type: CostTokenType) -> usize;
    /// Provides the ap change variable value of the current statement.
    fn ap_change_var_value(&self) -> usize;
}

/// Returns a precost value for a libfunc - the cost of non-step tokens.
/// This is a helper function to implement costing both for creating
/// gas equations and getting actual gas usage after having a solution.
pub fn core_libfunc_precost<Ops: CostOperations>(
    ops: &mut Ops,
    libfunc: &CoreConcreteLibfunc,
) -> Vec<Ops::CostType> {
    match libfunc {
        FunctionCall(FunctionCallConcreteLibfunc { function, .. }) => {
            let func_content_cost = CostTokenType::iter_precost()
                .map(|token| ops.function_token_cost(function, *token))
                .collect_vec()
                .into_iter()
                .reduce(|x, y| ops.add(x, y));
            vec![func_content_cost.unwrap()]
        }
        Bitwise(_) => {
            vec![ops.cost_token(1, CostTokenType::Bitwise)]
        }
        Ec(EcConcreteLibfunc::StateAddMul(_)) => {
            vec![ops.cost_token(1, CostTokenType::EcOp)]
        }
        BranchAlign(_) => {
            vec![statement_vars_cost(ops, CostTokenType::iter_precost())]
        }
        Pedersen(_) => {
            vec![ops.cost_token(1, CostTokenType::Pedersen)]
        }
        BuiltinCost(BuiltinCostConcreteLibfunc::BuiltinGetGas(_)) => {
            vec![
                ops.sub(ops.steps(0), statement_vars_cost(ops, CostTokenType::iter_precost())),
                ops.steps(0),
            ]
        }
        _ => libfunc.branch_signatures().iter().map(|_| ops.steps(0)).collect(),
    }
}

/// Returns a postcost value for a libfunc - the cost of step token.
/// This is a helper function to implement costing both for creating
/// gas equations and getting actual gas usage after having a solution.
pub fn core_libfunc_postcost<Ops: CostOperations, InfoProvider: InvocationCostInfoProvider>(
    ops: &mut Ops,
    libfunc: &CoreConcreteLibfunc,
    info_provider: &InfoProvider,
) -> Vec<Ops::CostType> {
    match libfunc {
        // For the case of function calls - assumes a variable for the cost of running from a
        // function entry point and on - while also adding the call cost.
        FunctionCall(FunctionCallConcreteLibfunc { function, .. }) => {
            let func_content_cost = ops.function_token_cost(function, CostTokenType::Const);
            vec![ops.add(ops.steps(2), func_content_cost)]
        }
        Bitwise(_) => {
            vec![ops.steps(2)]
        }
        Bool(BoolConcreteLibfunc::And(_)) => vec![ops.steps(0)],
        Bool(BoolConcreteLibfunc::Not(_)) => vec![ops.steps(1)],
        Bool(BoolConcreteLibfunc::Xor(_)) => vec![ops.steps(1)],
        Bool(BoolConcreteLibfunc::Or(_)) => vec![ops.steps(2)],
        Bool(BoolConcreteLibfunc::Equal(_)) => vec![ops.steps(2), ops.steps(3)],
        Ec(libfunc) => match libfunc {
            EcConcreteLibfunc::IsZero(_) => vec![ops.steps(1), ops.steps(1)],
            EcConcreteLibfunc::Neg(_) => vec![ops.steps(0)],
            EcConcreteLibfunc::StateAdd(_) => vec![ops.steps(10)],
            EcConcreteLibfunc::TryNew(_) => vec![ops.steps(7), ops.steps(7)],
            EcConcreteLibfunc::StateFinalize(_) => vec![ops.steps(12), ops.steps(6)],
            EcConcreteLibfunc::StateInit(_) => vec![ops.steps(8)],
            EcConcreteLibfunc::StateAddMul(_) => {
                vec![ops.steps(5)]
            }
            EcConcreteLibfunc::PointFromX(_) => vec![
                ops.steps(8), // Success.
                ops.steps(9), // Failure.
            ],
            EcConcreteLibfunc::UnwrapPoint(_) => vec![ops.steps(0)],
            EcConcreteLibfunc::Zero(_) => vec![ops.steps(0)],
        },
        Gas(GetGas(_)) => {
            vec![
                ops.sub(
                    ops.const_cost(ConstCost { steps: 3, holes: 0, range_checks: 1 }),
                    ops.statement_var_cost(CostTokenType::Const),
                ),
                ops.const_cost(ConstCost { steps: 4, holes: 0, range_checks: 1 }),
            ]
        }
        Gas(RefundGas(_)) => vec![ops.statement_var_cost(CostTokenType::Const)],
        BranchAlign(_) => {
            let ap_change = info_provider.ap_change_var_value();
            let burnt_cost = ops.statement_var_cost(CostTokenType::Const);
            vec![if ap_change == 0 {
                burnt_cost
            } else {
                ops.add(
                    burnt_cost,
                    ops.const_cost(ConstCost {
                        steps: 1,
                        holes: ap_change as i32,
                        range_checks: 0,
                    }),
                )
            }]
        }
        Array(ArrayConcreteLibfunc::New(_)) => vec![ops.steps(1)],
        Array(ArrayConcreteLibfunc::Append(libfunc)) => {
            vec![ops.steps(info_provider.type_size(&libfunc.ty) as i32)]
        }
        Array(ArrayConcreteLibfunc::PopFront(_)) => vec![ops.steps(2), ops.steps(3)],
        Array(ArrayConcreteLibfunc::At(libfunc)) => {
            if info_provider.type_size(&libfunc.ty) == 1 {
                vec![
                    ops.const_cost(ConstCost { steps: 6, holes: 0, range_checks: 1 }),
                    ops.const_cost(ConstCost { steps: 5, holes: 0, range_checks: 1 }),
                ]
            } else {
                vec![
                    ops.const_cost(ConstCost { steps: 7, holes: 0, range_checks: 1 }),
                    ops.const_cost(ConstCost { steps: 7, holes: 0, range_checks: 1 }),
                ]
            }
        }
        Array(ArrayConcreteLibfunc::Len(libfunc)) => {
            vec![ops.steps(if info_provider.type_size(&libfunc.ty) == 1 { 0 } else { 1 })]
        }
        Uint128(libfunc) => u128_libfunc_cost(ops, libfunc),
        Uint8(libfunc) => u8_libfunc_cost(ops, libfunc),
        Uint64(libfunc) => u64_libfunc_cost(ops, libfunc),
        Felt(libfunc) => felt_libfunc_cost(ops, libfunc),
        Drop(_) | Dup(_) | ApTracking(_) | UnwrapNonZero(_) | Mem(Rename(_)) => {
            vec![ops.steps(0)]
        }
        Box(libfunc) => match libfunc {
            BoxConcreteLibfunc::Into(libfunc) => {
                vec![ops.steps(1.max(info_provider.type_size(&libfunc.ty).try_into().unwrap()))]
            }
            BoxConcreteLibfunc::Unbox(_) => vec![ops.steps(0)],
        },
        Mem(StoreTemp(libfunc)) => {
            vec![ops.steps(info_provider.type_size(&libfunc.ty) as i32)]
        }
        Mem(StoreLocal(libfunc)) => {
            let size = info_provider.type_size(&libfunc.ty) as i32;
            vec![ops.const_cost(ConstCost { steps: size, holes: -size, range_checks: 0 })]
        }
        Mem(AllocLocal(libfunc) | AlignTemps(libfunc)) => {
            vec![ops.holes(info_provider.type_size(&libfunc.ty) as i32)]
        }

        Mem(FinalizeLocals(_)) | UnconditionalJump(_) => {
            vec![ops.steps(1)]
        }
        Enum(EnumConcreteLibfunc::Init(_)) => vec![ops.steps(1)],
        Enum(EnumConcreteLibfunc::Match(sig)) => {
            vec![ops.steps(1); sig.signature.branch_signatures.len()]
        }
        Struct(StructConcreteLibfunc::Construct(_) | StructConcreteLibfunc::Deconstruct(_)) => {
            vec![ops.steps(0)]
        }
        DictFeltTo(DictFeltToConcreteLibfunc::New(_)) => {
            vec![ops.steps(9)]
        }
        DictFeltTo(DictFeltToConcreteLibfunc::Read(_)) => {
            vec![
                ops.add(
                    ops.steps(3),
                    ops.cost_token(DICT_SQUASH_ACCESS_COST, CostTokenType::Const),
                ),
            ]
        }
        DictFeltTo(DictFeltToConcreteLibfunc::Write(_)) => {
            vec![
                ops.add(
                    ops.steps(3),
                    ops.cost_token(DICT_SQUASH_ACCESS_COST, CostTokenType::Const),
                ),
            ]
        }
        DictFeltTo(DictFeltToConcreteLibfunc::Squash(_)) => {
            // Dict squash have a fixed cost of 92 + `DICT_SQUASH_ACCESS_COST` for each dict access.
            // Only the fixed cost is charged here, so that we would alway be able to
            // call squash even if rnning out of gas. The cost of the proccesing of the
            // first key is `DICT_SQUASH_ACCESS_COST`, and each access for an existing
            // key costs only 12. In each read/write we charge `DICT_SQUASH_ACCESS_COST` gas and
            // `DICT_SQUASH_ACCESS_COST - 12` gas are refunded per each succesive access
            // in dict squash.
            vec![ops.steps(92)]
        }
        Pedersen(_) => {
            vec![ops.steps(2)]
        }
        BuiltinCost(builtin_libfunc) => match builtin_libfunc {
            BuiltinCostConcreteLibfunc::BuiltinGetGas(_) => {
                let cost_computation =
                    BuiltinCostGetGasLibfunc::cost_computation_steps(|token_type| {
                        info_provider.token_usages(token_type)
                    }) as i32;
                vec![
                    ops.sub(
                        ops.const_cost(ConstCost {
                            steps: cost_computation + 3,
                            holes: 0,
                            range_checks: 1,
                        }),
                        ops.statement_var_cost(CostTokenType::Const),
                    ),
                    ops.const_cost(ConstCost {
                        steps: cost_computation + 5,
                        holes: 0,
                        range_checks: 1,
                    }),
                ]
            }
            BuiltinCostConcreteLibfunc::GetBuiltinCosts(_) => vec![ops.steps(3)],
        },
        CoreConcreteLibfunc::StarkNet(libfunc) => starknet_libfunc_cost_base(ops, libfunc),
        CoreConcreteLibfunc::Nullable(libfunc) => match libfunc {
            NullableConcreteLibfunc::Null(_) => vec![ops.steps(0)],
            NullableConcreteLibfunc::IntoNullable(_) => vec![ops.steps(0)],
            NullableConcreteLibfunc::FromNullable(_) => vec![ops.steps(1), ops.steps(1)],
        },
        CoreConcreteLibfunc::Debug(_) => vec![ops.steps(1)],
    }
}

/// Returns the sum of statement variables for all the requested tokens.
fn statement_vars_cost<'a, Ops: CostOperations, TokenTypes: Iterator<Item = &'a CostTokenType>>(
    ops: &Ops,
    token_types: TokenTypes,
) -> Ops::CostType {
    token_types
        .map(|token_type| ops.statement_var_cost(*token_type))
        .reduce(|x, y| ops.add(x, y))
        .unwrap()
}

/// Returns costs for u8 libfuncs.
fn u8_libfunc_cost<Ops: CostOperations>(ops: &Ops, libfunc: &Uint8Concrete) -> Vec<Ops::CostType> {
    match libfunc {
        Uint8Concrete::Const(_) | Uint8Concrete::ToFelt(_) => vec![ops.steps(0)],
        Uint8Concrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd => {
                vec![
                    ops.const_cost(ConstCost { steps: 4, holes: 0, range_checks: 1 }),
                    ops.const_cost(ConstCost { steps: 5, holes: 0, range_checks: 1 }),
                ]
            }
            IntOperator::OverflowingSub => {
                vec![
                    ops.const_cost(ConstCost { steps: 3, holes: 0, range_checks: 1 }),
                    ops.const_cost(ConstCost { steps: 6, holes: 0, range_checks: 1 }),
                ]
            }
        },
        Uint8Concrete::LessThan(_) => {
            vec![
                ops.const_cost(ConstCost { steps: 3, holes: 0, range_checks: 1 }),
                ops.const_cost(ConstCost { steps: 5, holes: 0, range_checks: 1 }),
            ]
        }
        Uint8Concrete::Equal(_) => {
            vec![ops.steps(2), ops.steps(3)]
        }
        Uint8Concrete::LessThanOrEqual(_) => {
            vec![
                ops.const_cost(ConstCost { steps: 4, holes: 0, range_checks: 1 }),
                ops.const_cost(ConstCost { steps: 4, holes: 0, range_checks: 1 }),
            ]
        }
        Uint8Concrete::FromFelt(_) => {
            vec![
                ops.const_cost(ConstCost { steps: 4, holes: 0, range_checks: 2 }),
                ops.const_cost(ConstCost { steps: 10, holes: 0, range_checks: 3 }),
            ]
        }
    }
}

/// Returns costs for u64 libfuncs.
fn u64_libfunc_cost<Ops: CostOperations>(
    ops: &Ops,
    libfunc: &Uint64Concrete,
) -> Vec<Ops::CostType> {
    match libfunc {
        Uint64Concrete::Const(_) | Uint64Concrete::ToFelt(_) => vec![ops.steps(0)],
        Uint64Concrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd => {
                vec![
                    ops.const_cost(ConstCost { steps: 4, holes: 0, range_checks: 1 }),
                    ops.const_cost(ConstCost { steps: 5, holes: 0, range_checks: 1 }),
                ]
            }
            IntOperator::OverflowingSub => {
                vec![
                    ops.const_cost(ConstCost { steps: 3, holes: 0, range_checks: 1 }),
                    ops.const_cost(ConstCost { steps: 6, holes: 0, range_checks: 1 }),
                ]
            }
        },
        Uint64Concrete::LessThan(_) => {
            vec![
                ops.const_cost(ConstCost { steps: 3, holes: 0, range_checks: 1 }),
                ops.const_cost(ConstCost { steps: 5, holes: 0, range_checks: 1 }),
            ]
        }
        Uint64Concrete::Equal(_) => {
            vec![ops.steps(2), ops.steps(3)]
        }
        Uint64Concrete::LessThanOrEqual(_) => {
            vec![
                ops.const_cost(ConstCost { steps: 4, holes: 0, range_checks: 1 }),
                ops.const_cost(ConstCost { steps: 4, holes: 0, range_checks: 1 }),
            ]
        }
        Uint64Concrete::FromFelt(_) => {
            vec![
                ops.const_cost(ConstCost { steps: 4, holes: 0, range_checks: 2 }),
                ops.const_cost(ConstCost { steps: 10, holes: 0, range_checks: 3 }),
            ]
        }
    }
}

/// Returns costs for u128 libfuncs.
fn u128_libfunc_cost<Ops: CostOperations>(
    ops: &Ops,
    libfunc: &Uint128Concrete,
) -> Vec<Ops::CostType> {
    // TODO(orizi): When sierra_to_casm actually supports integers - fix costs.
    match libfunc {
        Uint128Concrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd | IntOperator::OverflowingSub => {
                vec![
                    ops.const_cost(ConstCost { steps: 3, holes: 0, range_checks: 1 }),
                    ops.const_cost(ConstCost { steps: 5, holes: 0, range_checks: 1 }),
                ]
            }
        },
        Uint128Concrete::Divmod(_) => {
            vec![ops.const_cost(ConstCost { steps: 11, holes: 0, range_checks: 4 })]
        }
        Uint128Concrete::WideMul(_) => {
            vec![ops.const_cost(ConstCost { steps: 23, holes: 0, range_checks: 9 })]
        }
        Uint128Concrete::Const(_) | Uint128Concrete::ToFelt(_) => {
            vec![ops.steps(0)]
        }
        Uint128Concrete::FromFelt(_) => {
            vec![
                ops.const_cost(ConstCost { steps: 2, holes: 0, range_checks: 1 }),
                ops.const_cost(ConstCost { steps: 11, holes: 0, range_checks: 3 }),
            ]
        }
        Uint128Concrete::IsZero(_) => {
            vec![ops.steps(1), ops.steps(1)]
        }
        Uint128Concrete::LessThan(_) => {
            vec![
                ops.const_cost(ConstCost { steps: 3, holes: 0, range_checks: 1 }),
                ops.const_cost(ConstCost { steps: 5, holes: 0, range_checks: 1 }),
            ]
        }
        Uint128Concrete::Equal(_) => {
            vec![ops.steps(2), ops.steps(3)]
        }
        Uint128Concrete::LessThanOrEqual(_) => {
            vec![
                ops.const_cost(ConstCost { steps: 4, holes: 0, range_checks: 1 }),
                ops.const_cost(ConstCost { steps: 4, holes: 0, range_checks: 1 }),
            ]
        }
    }
}

/// Returns costs for felt libfuncs.
fn felt_libfunc_cost<Ops: CostOperations>(ops: &Ops, libfunc: &FeltConcrete) -> Vec<Ops::CostType> {
    match libfunc {
        FeltConcrete::Const(_) | FeltConcrete::BinaryOperation(_) => vec![ops.steps(0)],
        FeltConcrete::IsZero(_) => {
            vec![ops.steps(1), ops.steps(1)]
        }
    }
}
