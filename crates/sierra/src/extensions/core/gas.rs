use crate::extensions::{
    Specialization, SpecializationBox, SpecializationError, Specializer, SpecializerBox,
};
use crate::program::{Identifier, TemplateArg};

struct GetGasSpecializer {}
impl Specializer for GetGasSpecializer {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError> {
        match tmpl_args {
            [TemplateArg::Value(count)] if *count > 0 => Ok(Box::new(GetGas { _count: *count })),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}
struct GetGas {
    _count: i64,
}
impl Specialization for GetGas {}

struct RefundGasSpecializer {}
impl Specializer for RefundGasSpecializer {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError> {
        match tmpl_args {
            [TemplateArg::Value(count)] if *count > 0 => Ok(Box::new(RefundGas { _count: *count })),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}
struct RefundGas {
    _count: i64,
}
impl Specialization for RefundGas {}

pub(super) fn extensions() -> [(Identifier, SpecializerBox); 2] {
    [
        (Identifier::Name("get_gas".into()), Box::new(GetGasSpecializer {})),
        (Identifier::Name("refund_gas".into()), Box::new(RefundGasSpecializer {})),
    ]
}
