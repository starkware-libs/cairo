use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use thiserror::Error;

use crate::ids::VarId;

#[cfg(test)]
#[path = "edit_state_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum EditStateError {
    #[error("Missing reference")]
    MissingReference(VarId),
    #[error("Overridden variable")]
    VariableOverride(VarId),
}
impl EditStateError {
    pub fn var_id(self) -> VarId {
        match self {
            EditStateError::MissingReference(var_id) => var_id,
            EditStateError::VariableOverride(var_id) => var_id,
        }
    }
}

/// Trait for editing the state of variables in a map.
pub trait EditState<V> {
    /// Removes the given ids from the map and return their values, failing if some id is missing.
    fn take_vars<'a>(
        &mut self,
        ids: impl ExactSizeIterator<Item = &'a VarId>,
    ) -> Result<Vec<V>, EditStateError>;

    /// Adds the given pairs to the map, failing if some variable is overridden.
    fn put_vars<'a>(
        &mut self,
        results: impl ExactSizeIterator<Item = (&'a VarId, V)>,
    ) -> Result<(), EditStateError>;
}

impl<V> EditState<V> for OrderedHashMap<VarId, V> {
    fn take_vars<'a>(
        &mut self,
        ids: impl ExactSizeIterator<Item = &'a VarId>,
    ) -> Result<Vec<V>, EditStateError> {
        let mut vals = Vec::with_capacity(ids.len());
        for id in ids {
            match self.swap_remove(id) {
                None => {
                    return Err(EditStateError::MissingReference(id.clone()));
                }
                Some(v) => {
                    vals.push(v);
                }
            }
        }
        Ok(vals)
    }

    fn put_vars<'a>(
        &mut self,
        results: impl ExactSizeIterator<Item = (&'a VarId, V)>,
    ) -> Result<(), EditStateError> {
        self.reserve(results.len());
        for (id, v) in results {
            if self.insert(id.clone(), v).is_some() {
                return Err(EditStateError::VariableOverride(id.clone()));
            }
        }
        Ok(())
    }
}
