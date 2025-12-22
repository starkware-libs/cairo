//! Utilities for parallel processing.
//!
//! This module provides utilities for parallel processing using salsa database.
//!
//! Will be removed when salsa provides direct support for parallel processing.

use rayon::iter::{FromParallelIterator, IntoParallelIterator, ParallelIterator};
use salsa::Database;

pub trait CloneableDatabase: Database + Send {
    fn dyn_clone(&self) -> Box<dyn CloneableDatabase>;
}
impl Clone for Box<dyn CloneableDatabase> {
    fn clone(&self) -> Self {
        self.dyn_clone()
    }
}

/// Parallel map function for processing elements in parallel using a salsa database.
pub fn par_map<F, T, R, C>(
    db: &dyn CloneableDatabase,
    inputs: impl IntoParallelIterator<Item = T>,
    op: F,
) -> C
where
    F: Fn(&dyn CloneableDatabase, T) -> R + Sync + Send,
    T: Send,
    R: Send + Sync,
    C: FromParallelIterator<R>,
{
    inputs
        .into_par_iter()
        .map_with(db.dyn_clone(), |db, element| op(db.as_ref(), element))
        .collect()
}
