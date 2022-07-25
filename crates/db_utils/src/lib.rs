#[macro_export]

// Defines an id struct for use with salsa interning.
// Interning is the process of representing a value as an id in a table.
// We usually denote the value type as "Long Id", and the id type as "Short Id" or just "Id".
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
