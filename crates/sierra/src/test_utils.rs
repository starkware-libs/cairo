use bimap::BiMap;
use itertools::chain;

use crate::ids::ConcreteTypeId;
use crate::program::{ConcreteTypeLongId, GenericArg};

pub fn build_bijective_mapping() -> BiMap<ConcreteTypeId, ConcreteTypeLongId> {
    let mut elements = BiMap::new();
    elements.insert("T".into(), as_type_long_id("T", &[]));
    elements.insert("u128".into(), as_type_long_id("u128", &[]));
    elements.insert("felt".into(), as_type_long_id("felt", &[]));
    elements.insert("Tuple<>".into(), as_named_type_long_id("Struct", "Tuple", &[]));
    elements.insert(
        "U128AndFelt".into(),
        as_named_type_long_id("Struct", "U128AndFelt", &["u128", "felt"]),
    );
    elements.insert("Option".into(), as_named_type_long_id("Enum", "Option", &["felt", "Tuple<>"]));
    elements.insert("NonZeroFelt".into(), as_type_long_id("NonZero", &["felt"]));
    elements.insert("NonZeroU128".into(), as_type_long_id("NonZero", &["u128"]));
    elements.insert("ArrayFelt".into(), as_type_long_id("Array", &["felt"]));
    elements.insert("ArrayU128".into(), as_type_long_id("Array", &["u128"]));
    elements.insert("UninitializedFelt".into(), as_type_long_id("Uninitialized", &["felt"]));
    elements.insert("Uninitializedu128".into(), as_type_long_id("Uninitialized", &["u128"]));
    elements.insert("GasBuiltin".into(), as_type_long_id("GasBuiltin", &[]));
    elements.insert("RangeCheck".into(), as_type_long_id("RangeCheck", &[]));
    elements.insert("System".into(), as_type_long_id("System", &[]));
    elements.insert("StorageAddress".into(), as_type_long_id("StorageAddress", &[]));
    elements
}

fn as_type_long_id(name: &str, args: &[&str]) -> ConcreteTypeLongId {
    ConcreteTypeLongId {
        generic_id: name.into(),
        generic_args: args.iter().map(|s| GenericArg::Type(ConcreteTypeId::from(*s))).collect(),
    }
}

fn as_named_type_long_id(genetic_name: &str, user_name: &str, args: &[&str]) -> ConcreteTypeLongId {
    ConcreteTypeLongId {
        generic_id: genetic_name.into(),
        generic_args: chain!(
            [GenericArg::UserType(user_name.into())],
            args.iter().map(|s| GenericArg::Type(ConcreteTypeId::from(*s)))
        )
        .collect(),
    }
}
