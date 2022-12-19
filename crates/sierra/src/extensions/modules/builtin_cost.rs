use convert_case::Casing;

use super::gas::GasBuiltinType;
use super::range_check::RangeCheckType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibFunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};

/// Represents different type of costs.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum CostTokenType {
    /// A single Cairo step, or some cost which is equivalent to it.
    Step,
    /// One invocation of the pedersen hash function.
    Pedersen,
}
impl CostTokenType {
    pub fn iter() -> std::slice::Iter<'static, Self> {
        [CostTokenType::Step, CostTokenType::Pedersen].iter()
    }

    /// Returns the name of the token type, in snake_case.
    pub fn name(&self) -> String {
        match self {
            CostTokenType::Step => "step",
            CostTokenType::Pedersen => "pedersen",
        }
        .into()
    }

    pub fn camel_case_name(&self) -> String {
        self.name().to_case(convert_case::Case::UpperCamel)
    }

    pub fn offset_in_builtin_costs(&self) -> i16 {
        match self {
            CostTokenType::Step => panic!("offset_in_builtin_costs is not supported for 'Step'."),
            CostTokenType::Pedersen => 0,
        }
    }
}

/// Represents the cost of a single invocation of a builtin.
#[derive(Default)]
pub struct BuiltinCostsType {}
impl NoGenericArgsGenericType for BuiltinCostsType {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("BuiltinCosts");

    fn specialize(&self) -> Self::Concrete {
        InfoOnlyConcreteType {
            info: TypeInfo {
                long_id: Self::concrete_type_long_id(&[]),
                storable: true,
                droppable: true,
                duplicatable: true,
                size: 1,
            },
        }
    }
}

define_libfunc_hierarchy! {
    pub enum BuiltinCostLibFunc {
        BuiltinGetGas(BuiltinCostGetGasLibFunc),
    }, BuiltinCostConcreteLibFunc
}

/// LibFunc for getting gas to be used by a builtin.
#[derive(Default)]
pub struct BuiltinCostGetGasLibFunc {}
impl BuiltinCostGetGasLibFunc {
    /// Returns the maximal number of steps required for the computation of the requested cost.
    /// The number of steps is also the change in `ap` (every step includes `ap++`).
    pub fn cost_computation_max_steps() -> usize {
        (CostTokenType::iter().len() - 1) * 3
    }
}

impl NoGenericArgsGenericLibFunc for BuiltinCostGetGasLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("get_gas_all");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let builtin_costs_type = context.get_concrete_type(BuiltinCostsType::id(), &[])?;
        Ok(LibFuncSignature {
            param_signatures: vec![
                ParamSignature {
                    ty: range_check_type.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(gas_builtin_type.clone()),
                ParamSignature::new(builtin_costs_type),
            ],
            branch_signatures: vec![
                // Success:
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: range_check_type.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty: gas_builtin_type.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure:
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: range_check_type,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty: gas_builtin_type,
                            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
