use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_filesystem::ids::SpanInFile;
use cairo_lang_lowering::ids::LocationId;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::CloneableDatabase;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::Itertools;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use salsa::Database;

use crate::debug_info::statements_locations::statements_functions::{
    maybe_containing_function_identifier, maybe_containing_function_identifier_for_tests,
};
use crate::debug_info::{StatementsFunctions, StatementsSourceCodeLocations, maybe_code_location};

pub mod statements_code_locations;
pub mod statements_functions;

#[cfg(test)]
#[path = "statements_locations_test.rs"]
mod test;

/// The locations in the Cairo source code which caused a statement to be generated.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct StatementsLocations<'db> {
    pub locations: UnorderedHashMap<StatementIdx, Vec<StableLocation<'db>>>,
}

impl<'db> StatementsLocations<'db> {
    /// Creates a new [StatementsLocations] object from a list of [`Option<LocationId<'db>>`].
    pub fn from_locations_vec(
        db: &'db dyn Database,
        locations: Vec<Option<LocationId<'db>>>,
    ) -> Self {
        let mut cache = UnorderedHashMap::<LocationId<'db>, Vec<StableLocation<'db>>>::default();
        Self {
            locations: locations
                .into_iter()
                .enumerate()
                .filter_map(|(i, loc)| {
                    let loc = loc?;
                    Some((
                        StatementIdx(i),
                        cache.entry(loc).or_insert_with(|| loc.all_locations(db)).clone(),
                    ))
                })
                .collect(),
        }
    }
    /// Builds a map between each Sierra statement index and a string representation of the Cairo
    /// function that it was generated from. It is used for places
    /// without DB access such as the profiler.
    // TODO(Gil): Add a db access to the profiler and remove this function.
    pub fn get_statements_functions_map_for_tests(
        &self,
        db: &dyn Database,
    ) -> UnorderedHashMap<StatementIdx, String> {
        self.locations
            .iter_sorted()
            .filter_map(|(statement_idx, stable_locations)| {
                maybe_containing_function_identifier_for_tests(
                    db,
                    *stable_locations.first().unwrap(),
                )
                .map(|function_identifier| (*statement_idx, function_identifier))
            })
            .collect()
    }

    /// Creates a new [StatementsFunctions] struct using [StatementsLocations].
    pub fn extract_statements_functions(&self, db: &dyn CloneableDatabase) -> StatementsFunctions {
        StatementsFunctions {
            statements_to_functions_map: self
                .locations
                .iter_sorted()
                .collect_vec()
                .into_par_iter()
                .map_with(db.dyn_clone(), |db, (statement_idx, stable_locations)| {
                    (
                        *statement_idx,
                        stable_locations
                            .iter()
                            .filter_map(|s| maybe_containing_function_identifier(db.as_ref(), *s))
                            .collect(),
                    )
                })
                .collect(),
        }
    }

    /// Creates a new [StatementsSourceCodeLocations] struct using [StatementsLocations].
    pub fn extract_statements_source_code_locations(
        &self,
        db: &dyn CloneableDatabase,
    ) -> StatementsSourceCodeLocations {
        StatementsSourceCodeLocations {
            statements_to_code_location_map: self
                .locations
                .iter_sorted()
                .collect_vec()
                .into_par_iter()
                .map_with(db.dyn_clone(), |db, (statement_idx, stable_locations)| {
                    (
                        *statement_idx,
                        stable_locations
                            .iter()
                            .filter_map(|s| maybe_code_location(db.as_ref(), *s))
                            .collect(),
                    )
                })
                .collect(),
        }
    }

    /// Returns the diagnostic location matching the user code corresponding to the Sierra statement
    /// index.
    pub fn statement_diagnostic_location(
        &self,
        db: &'db dyn Database,
        stmt_idx: StatementIdx,
    ) -> Option<SpanInFile<'db>> {
        // Note that the `last` is used here as the call site is the most relevant location.
        self.locations
            .get(&stmt_idx)
            .and_then(|stmt_locs| stmt_locs.last())
            .map(|loc| loc.span_in_file(db).user_location(db))
    }
}
