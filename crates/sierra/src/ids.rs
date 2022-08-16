use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

fn id_from_string(s: &str) -> u64 {
    // TODO(ilya, 10/10/2022): Fix https://github.com/starkware-libs/cairo2/issues/45.
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}

macro_rules! define_identity {
    ($doc:literal, $derives:tt, $type_name:ident) => {
        #[doc=$doc]
        #[derive $derives]

        pub struct $type_name {
            pub id: u64,
            /// Optional name for testing and debugging.
            pub debug_name: Option<String>,
        }

        impl $type_name {
            pub fn new(id: u64) -> Self {
                $type_name{id, debug_name: None}
            }

            pub fn from_string(name: impl Into<String>) -> Self {
                let s: String = name.into();
                $type_name{id: id_from_string(&s), debug_name: Some(s)}
            }
        }
        impl From<&str> for $type_name {
            fn from(name: &str) -> Self {
                Self::from_string(name.to_string())
            }
        }
        impl From<String> for $type_name {
            fn from(name: String) -> Self {
                Self::from_string(name)
            }
        }
        impl From<u64> for $type_name {
            fn from(id: u64) -> Self {
                Self::new(id)
            }
        }
    };
}

define_identity!(
    "The identity of a generic extension",
    (Clone, Debug, Eq, Hash, PartialEq),
    GenericExtensionId
);

define_identity!(
    "The identity of a concrete extension.",
    (Clone, Debug, Eq, Hash, PartialEq),
    ConcreteExtensionId
);

define_identity!("The identity of a user function.", (Clone, Debug, Eq, PartialEq), FunctionId);

define_identity!("The identity of a variable.", (Clone, Debug, Eq, Hash, PartialEq), VarId);

define_identity!("The identity of a generic type.", (Clone, Debug, Eq, PartialEq), GenericTypeId);

define_identity!("The identity of a concrete type.", (Clone, Debug, Eq, PartialEq), ConcreteTypeId);
