use smol_str::SmolStr;

const fn id_from_string(s: &str) -> u64 {
    // TODO(ilya, 10/10/2022): Fix https://github.com/starkware-libs/cairo2/issues/45.
    let mut hash = 0xcbf29ce484222325;
    let prime = 0x00000100000001B3;

    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        hash ^= bytes[i] as u64;
        hash = hash.wrapping_mul(prime);
        i += 1;
    }
    hash
}

macro_rules! define_identity {
    ($doc:literal, $type_name:ident) => {
        #[doc=$doc]
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct $type_name {
            pub id: u64,
            /// Optional name for testing and debugging.
            pub debug_name: Option<SmolStr>,
        }

        impl $type_name {
            pub fn new(id: u64) -> Self {
                $type_name { id, debug_name: None }
            }

            // TODO(lior): Remove this function once issue #45 is resolved. Use new() instead.
            pub fn from_usize(id: usize) -> Self {
                Self::new(id.try_into().unwrap())
            }

            pub const fn new_inline(name: &'static str) -> Self {
                $type_name { id: id_from_string(name), debug_name: Some(SmolStr::new_inline(name)) }
            }

            pub fn from_string(name: impl Into<SmolStr>) -> Self {
                let s: SmolStr = name.into();
                $type_name { id: id_from_string(&s), debug_name: Some(s) }
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

define_identity!("The identity of a generic library function", GenericLibFuncId);

define_identity!("The identity of a concrete library function.", ConcreteLibFuncId);

define_identity!("The identity of a user function.", FunctionId);

define_identity!("The identity of a variable.", VarId);

define_identity!("The identity of a generic type.", GenericTypeId);

define_identity!("The identity of a concrete type.", ConcreteTypeId);
