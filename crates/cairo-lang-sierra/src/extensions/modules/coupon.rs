use crate::extensions::lib_func::SignatureSpecializationContext;
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{ConcreteType, NamedType, SpecializationError};
use crate::ids::{ConcreteTypeId, FunctionId, GenericTypeId};
use crate::program::GenericArg;

/// Coupon type `Coupon<function>` (`function::Coupon`) which represents that the cost of a
/// function was paid, without calling the function yet.
/// Using the coupon the function can be called without paying the cost.
#[derive(Default)]
pub struct CouponType {}

impl NamedType for CouponType {
    type Concrete = CouponConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Coupon");

    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let function_id = match args {
            [GenericArg::UserFunc(function_id)] => Ok(function_id.clone()),
            [_] => Err(SpecializationError::UnsupportedGenericArg),
            _ => Err(SpecializationError::WrongNumberOfGenericArgs),
        }?;

        let long_id = Self::concrete_type_long_id(args);
        Ok(Self::Concrete {
            info: TypeInfo {
                long_id,
                duplicatable: false,
                droppable: true,
                storable: true,
                zero_sized: true,
            },
            function_id,
        })
    }
}

/// Returns the type `Coupon<func>`.
pub fn coupon_ty(
    context: &dyn SignatureSpecializationContext,
    function_id: FunctionId,
) -> Result<ConcreteTypeId, SpecializationError> {
    context.get_concrete_type(CouponType::id(), &[GenericArg::UserFunc(function_id)])
}

/// Concrete type information for `Coupon<function>`.
pub struct CouponConcreteType {
    pub info: TypeInfo,
    pub function_id: FunctionId,
}
impl ConcreteType for CouponConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}
