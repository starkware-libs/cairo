#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
include!("../with_std.rs");

#[cfg(not(feature = "std"))]
include!("../without_std.rs");

/// Prelude of common useful imports.
///
/// This should include only things which are in the normal std prelude.
pub mod prelude {
    pub use crate::borrow::ToOwned;
    pub use crate::boxed::Box;
    pub use crate::clone::Clone;
    pub use crate::cmp::{Eq, PartialEq, Reverse};
    pub use crate::iter::IntoIterator;
    // Re-export `vec!` macro here, but not in `std` mode, since
    // std's prelude already brings `vec!` into the scope.
    #[cfg(not(feature = "std"))]
    pub use crate::vec;
    pub use crate::vec::Vec;
}

/// Feature gate some code that should only be run when `std` feature is enabled.
///
/// # Example
///
/// ```
/// use sp_std::if_std;
///
/// if_std! {
///     // This code is only being compiled and executed when the `std` feature is enabled.
///     println!("Hello native world");
/// }
/// ```
#[cfg(feature = "std")]
#[macro_export]
macro_rules! if_std {
	( $( $code:tt )* ) => {
		$( $code )*
	}
}

#[cfg(not(feature = "std"))]
#[macro_export]
macro_rules! if_std {
    ($($code:tt)*) => {};
}
