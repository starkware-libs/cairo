//! General utilities.

#[macro_export]

// Defines a short id struct for use with salsa interning.
// Interning is the process of representing a value as an id in a table.
// We usually denote the value type as "long id", and the id type as "short id" or just "id".
// Example:
//   A function's long id may be the module in which it is defined and its name. The function is
//   assigned a sequential integer (salsa::InternId) which will be its short id. Salsa will hold a
//   table to translate between the two representations. Note that a long id of an entity will
//   usually include the short id of the entity's parent.
macro_rules! define_short_id {
    ($short_id:ident) => {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        pub struct $short_id(salsa::InternId);
        impl salsa::InternKey for $short_id {
            fn from_intern_id(salsa_id: salsa::InternId) -> Self {
                Self(salsa_id)
            }

            fn as_intern_id(&self) -> salsa::InternId {
                self.0
            }
        }
    };
}
