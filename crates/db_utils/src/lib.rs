#[macro_export]

// Defines a short ID struct for use with salsa interning.
// Interning is the process of representing a value as an id in a table.
// We usually denote the value type as "Long Id", and the id type as "Short Id" or just "Id".
// Example:
//   Ilya's long ID is "Ilya".
//   Ilya's short ID is his israeli identity number, a more succinct ID.
//   Ilya might have a definiton: a handsome smart guy working at Starkware, etc...
//   But this is not his ID.
macro_rules! intern_id {
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
