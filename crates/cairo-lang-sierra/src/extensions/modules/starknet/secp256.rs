use super::secp256k1::Secp256K1EcLibfunc;
use super::secp256r1::Secp256R1EcLibfunc;
use crate::define_libfunc_hierarchy;
use crate::extensions::NoGenericArgsGenericType;
use crate::ids::GenericTypeId;

#[derive(Default)]
pub struct Secp256EcPointType {}
impl NoGenericArgsGenericType for Secp256EcPointType {
    const ID: GenericTypeId = GenericTypeId::new_inline("Secp256EcPoint");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 1;
}

define_libfunc_hierarchy! {
    pub enum Secp256EcLibfunc {
         Secp256K1(Secp256K1EcLibfunc),
         Secp256R1(Secp256R1EcLibfunc),
    }, Secp256EcConcreteLibfunc
}
