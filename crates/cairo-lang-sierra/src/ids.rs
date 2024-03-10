use derivative::Derivative;
use num_bigint::BigUint;
use num_traits::ToPrimitive;
use serde::{Deserialize, Serialize};
use sha3::{Digest, Keccak256};
use smol_str::SmolStr;

macro_rules! define_generic_identity {
    ($doc:literal, $type_name:ident) => {
        #[doc=$doc]
        #[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
        pub struct $type_name(pub SmolStr);
        impl $type_name {
            pub const fn new_inline(name: &'static str) -> Self {
                Self(SmolStr::new_inline(name))
            }

            pub fn from_string(name: impl Into<SmolStr>) -> Self {
                Self(name.into())
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
        impl From<SmolStr> for $type_name {
            fn from(name: SmolStr) -> Self {
                Self::from_string(name)
            }
        }
    };
}

define_generic_identity!("The identity of a generic library function", GenericLibfuncId);

define_generic_identity!("The identity of a generic type.", GenericTypeId);

macro_rules! define_identity {
    ($doc:literal, $type_name:ident) => {
        #[doc=$doc]
        #[derive(Clone, Debug, Derivative, Serialize, Deserialize)]
        #[derivative(Eq, Hash, PartialEq)]
        pub struct $type_name {
            pub id: u64,
            /// Optional name for testing and debugging.
            #[derivative(Hash = "ignore")]
            #[derivative(PartialEq = "ignore")]
            pub debug_name: Option<SmolStr>,
        }
        impl $type_name {
            pub fn new(id: u64) -> Self {
                Self { id, debug_name: None }
            }

            pub fn from_string(name: impl Into<SmolStr>) -> Self {
                let s: SmolStr = name.into();
                Self { id: const_fnv1a_hash::fnv1a_hash_str_64(&s), debug_name: Some(s) }
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
        impl From<SmolStr> for $type_name {
            fn from(name: SmolStr) -> Self {
                Self::from_string(name)
            }
        }
        impl From<u64> for $type_name {
            fn from(id: u64) -> Self {
                Self::new(id)
            }
        }
        impl salsa::InternKey for $type_name {
            fn from_intern_id(salsa_id: salsa::InternId) -> Self {
                Self::new(salsa_id.as_u32() as u64)
            }

            fn as_intern_id(&self) -> salsa::InternId {
                let id_usize: usize = self.id.try_into().unwrap();
                id_usize.into()
            }
        }
    };
}

define_identity!("The identity of a concrete library function.", ConcreteLibfuncId);

define_identity!("The identity of a user function.", FunctionId);

define_identity!("The identity of a variable.", VarId);

define_identity!("The identity of a concrete type.", ConcreteTypeId);

/// The identity of a user type.
#[derive(Clone, Debug, Derivative, Serialize, Deserialize)]
#[derivative(Eq, Hash, PartialEq)]
pub struct UserTypeId {
    pub id: BigUint,
    /// Optional name for testing and debugging.
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub debug_name: Option<SmolStr>,
}
impl UserTypeId {
    pub fn from_string(name: impl Into<SmolStr>) -> Self {
        let s: SmolStr = name.into();
        // TODO(orizi): Extract Keccak into felt252 implementation and use it at the starknet
        // crate as well.
        let mut hasher = Keccak256::new();
        hasher.update(s.as_bytes());
        let mut result = hasher.finalize();
        // Truncate result to 250 bits.
        *result.first_mut().unwrap() &= 3;
        let id = BigUint::from_bytes_be(&result);
        Self { id, debug_name: Some(s) }
    }
}
impl From<&str> for UserTypeId {
    fn from(name: &str) -> Self {
        Self::from_string(name.to_string())
    }
}
impl From<String> for UserTypeId {
    fn from(name: String) -> Self {
        Self::from_string(name)
    }
}
impl salsa::InternKey for UserTypeId {
    fn from_intern_id(salsa_id: salsa::InternId) -> Self {
        Self { id: salsa_id.as_usize().into(), debug_name: None }
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.id.to_usize().unwrap().into()
    }
}
