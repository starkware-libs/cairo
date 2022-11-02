use bimap::BiMap;
use itertools::chain;

use crate::ids::ConcreteTypeId;
use crate::program::{ConcreteTypeLongId, GenericArg};

pub fn build_bijective_mapping() -> BiMap<ConcreteTypeId, ConcreteTypeLongId> {
    let mut elements = BiMap::new();
    elements.insert("T".into(), as_type_long_id("T", &[]));
    elements.insert("uint128".into(), as_type_long_id("uint128", &[]));
    elements.insert("felt".into(), as_type_long_id("felt", &[]));
    elements.insert("Option".into(), {
        // TODO(yuval): change the second felt to unit type.
        let mut long_id = as_type_long_id("Enum", &["felt", "felt"]);
        long_id.generic_args =
            chain!([GenericArg::UserType("Option".into())], long_id.generic_args).collect();
        long_id
    });
    elements.insert("NonZeroFelt".into(), as_type_long_id("NonZero", &["felt"]));
    elements.insert("NonZeroUint128".into(), as_type_long_id("NonZero", &["uint128"]));
    elements.insert("ArrayFelt".into(), as_type_long_id("Array", &["felt"]));
    elements.insert("ArrayUint128".into(), as_type_long_id("Array", &["uint128"]));
    elements.insert("UninitializedFelt".into(), as_type_long_id("Uninitialized", &["felt"]));
    elements.insert("UninitializedUint128".into(), as_type_long_id("Uninitialized", &["uint128"]));
    elements.insert("GasBuiltin".into(), as_type_long_id("GasBuiltin", &[]));
    elements.insert("RangeCheck".into(), as_type_long_id("RangeCheck", &[]));
    elements
}

fn as_type_long_id(name: &str, args: &[&str]) -> ConcreteTypeLongId {
    ConcreteTypeLongId {
        generic_id: name.into(),
        generic_args: args.iter().map(|s| GenericArg::Type(ConcreteTypeId::from(*s))).collect(),
    }
}
