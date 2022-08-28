use crate::define_type_hierarchy;
use crate::extensions::{ConcreteType, NamedType, SpecializationError};
use crate::ids::{ConcreteLibFuncId, ConcreteTypeId, GenericTypeId};
use crate::program::GenericArg;

define_type_hierarchy! {
    pub enum CostType {
        LibFuncSet(LibFuncsCostType),
        Max(MaxCostType),
    }, CostTypeConcrete
}

/// Type for the cost of a set of libfuncs.
#[derive(Default)]
pub struct LibFuncsCostType {}
impl NamedType for LibFuncsCostType {
    type Concrete = LibFuncsCostConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("LibFuncsCost");
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        let mut libfuncs = vec![];
        for arg in args {
            if let GenericArg::LibFunc(libfunc) = arg {
                libfuncs.push(libfunc.clone());
            } else {
                return Err(SpecializationError::UnsupportedGenericArg);
            }
        }
        Ok(LibFuncsCostConcreteType { libfuncs })
    }
}
pub struct LibFuncsCostConcreteType {
    pub libfuncs: Vec<ConcreteLibFuncId>,
}
impl ConcreteType for LibFuncsCostConcreteType {}

/// Type for the cost of the maximum of a set of sets of costs.
#[derive(Default)]
pub struct MaxCostType {}
impl NamedType for MaxCostType {
    type Concrete = MaxCostConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("MaxCost");
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        let mut types = vec![];
        for arg in args {
            if let GenericArg::Type(ty) = arg {
                types.push(ty.clone());
            } else {
                return Err(SpecializationError::UnsupportedGenericArg);
            }
        }
        Ok(MaxCostConcreteType { types })
    }
}
pub struct MaxCostConcreteType {
    pub types: Vec<ConcreteTypeId>,
}
impl ConcreteType for MaxCostConcreteType {}
