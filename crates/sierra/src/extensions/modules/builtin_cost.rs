use convert_case::Casing;

use super::gas::GasBuiltinType;
use super::range_check::RangeCheckType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, GenericLibFunc, GenericType, NamedType, OutputVarReferenceInfo,
    SignatureBasedConcreteLibFunc, SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};
use crate::program::{ConcreteTypeLongId, GenericArg};

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
}

/// Represents the cost of a single invocation of a builtin.
pub struct BuiltinCostType {
    token_type: CostTokenType,
}
impl BuiltinCostType {
    fn id_from_token_type(token_type: CostTokenType) -> GenericTypeId {
        GenericTypeId::from_string(&format!("{}BuiltinCost", token_type.camel_case_name()))
    }
}
impl GenericType for BuiltinCostType {
    type Concrete = BuiltinCostConcreteType;

    fn by_id(id: &GenericTypeId) -> Option<Self> {
        for token_type in CostTokenType::iter() {
            if *id == Self::id_from_token_type(*token_type) {
                return Some(Self { token_type: *token_type });
            }
        }
        None
    }

    fn specialize(
        &self,
        _context: &dyn crate::extensions::type_specialization_context::TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        if !args.is_empty() {
            return Err(SpecializationError::WrongNumberOfGenericArgs);
        }

        Ok(BuiltinCostConcreteType {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: Self::id_from_token_type(self.token_type),
                    generic_args: vec![],
                },
                storable: true,
                droppable: true,
                duplicatable: true,
                size: 1,
            },
            token_type: self.token_type,
        })
    }
}

pub struct BuiltinCostConcreteType {
    pub info: TypeInfo,
    // TODO: is this needed?
    pub token_type: CostTokenType,
}

impl ConcreteType for BuiltinCostConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum BuiltinCostLibFunc {
        BuiltinGetGas(BuiltinCostGetGasLibFunc),
    }, BuiltinCostConcreteLibFunc
}

/// LibFunc for getting gas to be used by a builtin.
pub struct BuiltinCostGetGasLibFunc {
    token_type: CostTokenType,
}
impl GenericLibFunc for BuiltinCostGetGasLibFunc {
    type Concrete = BuiltinGetGasConcreteLibFunc;

    fn by_id(id: &GenericLibFuncId) -> Option<Self> {
        for token_type in CostTokenType::iter() {
            if *id == GenericLibFuncId::from_string(format!("{}_get_gas", token_type.name())) {
                return Some(Self { token_type: *token_type });
            }
        }
        None
    }

    fn specialize(
        &self,
        context: &dyn crate::extensions::lib_func::SpecializationContext,
        args: &[crate::program::GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(BuiltinGetGasConcreteLibFunc {
            signature: self.specialize_signature(context.upcast(), args)?,
            token_type: self.token_type,
        })
    }

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        if !args.is_empty() {
            return Err(SpecializationError::WrongNumberOfGenericArgs);
        }

        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let builtin_cost_type =
            context.get_concrete_type(BuiltinCostType::id_from_token_type(self.token_type), &[])?;
        Ok(LibFuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type.clone()),
                ParamSignature::new(gas_builtin_type.clone()),
                ParamSignature::new(builtin_cost_type),
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
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known(2), // TODO(lior): Check/fix.
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
                    ap_change: SierraApChange::Known(3), // TODO(lior): Check/fix.
                },
            ],
            fallthrough: Some(0),
        })
    }
}

pub struct BuiltinGetGasConcreteLibFunc {
    pub signature: LibFuncSignature,
    pub token_type: CostTokenType,
}
impl SignatureBasedConcreteLibFunc for BuiltinGetGasConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}
