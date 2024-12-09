//! Utility crate that exports internal corelib function.
//! This crate is compiled with an older edition that does not enforce visibity rules.

pub mod dummy {
    pub use core::internal::bounded_int::{
        BoundedInt, add, constrain, div_rem, sub, AddHelper, ConstrainHelper, DivRemHelper,
        SubHelper,
    };

    pub use core::integer::upcast;
}
