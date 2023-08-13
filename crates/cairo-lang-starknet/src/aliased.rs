/// An alias wrapper for a type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Aliased<T> {
    pub value: T,
    pub alias: String,
}
impl<T> Aliased<T> {
    /// Maps an aliased value to another aliased value with the same alias.
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Aliased<U> {
        Aliased { value: f(self.value), alias: self.alias }
    }

    /// Maps an aliased value to another aliased value with the same alias.
    /// Returns `None` if the mapping fails.
    pub fn try_map<U, F: FnOnce(T) -> Option<U>>(self, f: F) -> Option<Aliased<U>> {
        Some(Aliased { value: f(self.value)?, alias: self.alias })
    }
}
