use bimap::BiMap;

use crate::ids::ConcreteTypeId;
use crate::program::{ConcreteTypeLongId, GenericArg};

pub fn build_bijective_mapping() -> BiMap<ConcreteTypeId, ConcreteTypeLongId> {
    let mut elements = BiMap::new();
    elements.insert("T".into(), as_type_long_id("T", &[]));
    elements.insert("int".into(), as_type_long_id("int", &[]));
    elements.insert("felt".into(), as_type_long_id("felt", &[]));
    elements.insert("NonZeroFelt".into(), as_type_long_id("NonZero", &["felt"]));
    elements.insert("NonZeroInt".into(), as_type_long_id("NonZero", &["int"]));
    elements.insert("ArrayFelt".into(), as_type_long_id("Array", &["felt"]));
    elements.insert("ArrayInt".into(), as_type_long_id("Array", &["int"]));
    elements.insert("UninitializedFelt".into(), as_type_long_id("Uninitialized", &["felt"]));
    elements.insert("UninitializedInt".into(), as_type_long_id("Uninitialized", &["int"]));
    elements.insert("GasBuiltin".into(), as_type_long_id("GasBuiltin", &[]));
    elements
}

fn as_type_long_id(name: &str, args: &[&str]) -> ConcreteTypeLongId {
    ConcreteTypeLongId {
        generic_id: name.into(),
        generic_args: args.iter().map(|s| GenericArg::Type(ConcreteTypeId::from(*s))).collect(),
    }
}
