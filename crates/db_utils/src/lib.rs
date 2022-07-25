#[macro_export]

// Defines an ID struct for use with salsa interning.
// Interning is the process of representing a value as an id in a table.
// We usually denote the value type as "Long Id", and the id type as "Short Id" or just "Id".
// Example:
//   Ilya's long ID is "Ilya".
//   Ilya's short ID is his israeli identity number, a more succinct ID.
//   Ilya might have a definiton: a handsome smart guy working at Starkware, etc...
//   But this is not his ID.
macro_rules! intern_id {
    ($id:ident) => {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        pub struct $id(salsa::InternId);
        impl salsa::InternKey for $id {
            fn from_intern_id(id: salsa::InternId) -> Self {
                Self(id)
            }

            fn as_intern_id(&self) -> salsa::InternId {
                self.0
            }
        }
    };
}
