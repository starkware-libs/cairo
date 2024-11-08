use std::collections::HashSet;
use std::num::NonZero;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileId;
use itertools::Itertools;
use lsp_types::Url;
use tracing::info_span;

use crate::lang::db::AnalysisDatabase;
use crate::lang::lsp::LsProtoGroup;

#[cfg(test)]
#[path = "file_batches_test.rs"]
mod test;

/// Finds all analyzable files in `db` and constructs `n` batches of them to be processed.
///
/// Files specified in `open_files` will be prioritised and distributed evenly among the batches.
#[tracing::instrument(skip(db, open_files))]
pub fn make_file_batches(
    db: &AnalysisDatabase,
    open_files: &HashSet<Url>,
    n: NonZero<usize>,
) -> Vec<Vec<FileId>> {
    let open: Vec<FileId> = info_span!("find_open_files")
        .in_scope(|| open_files.iter().filter_map(|uri| db.file_for_url(uri)).dedup().collect());

    // NOTE: It would be tempting (and actually was done like so in the past) to immediately
    //   schedule refreshing diagnostics for the open files here to deliver results faster.
    //   However, at the time of writing this code, our profiling of the biggest Cairo codebases
    //   found out that finding other files is a very negligible (few milliseconds) process.
    //   Getting them here makes the rest of diagnostics code much simpler.

    let rest: Vec<FileId> = info_span!("find_other_files").in_scope(|| {
        let mut rest_of_files: HashSet<FileId> = HashSet::default();
        for crate_id in db.crates() {
            for module_id in db.crate_modules(crate_id).iter() {
                // Schedule only module main files for refreshing.
                // All other related files will be refreshed along with it in a single job.
                if let Ok(file) = db.module_main_file(*module_id) {
                    if !open.contains(&file) {
                        rest_of_files.insert(file);
                    }
                }
            }
        }
        Vec::from_iter(rest_of_files)
    });

    make_batches(open, rest, n)
}

/// Returns `n` optimally distributed batches of the `primary` and `secondary` vectors, merged.
///
/// All items from the `primary` vector will always be before any `secondary` item.
/// The best care will be taken for all batches to have the same length.
fn make_batches<T: Copy>(primary: Vec<T>, secondary: Vec<T>, n: NonZero<usize>) -> Vec<Vec<T>> {
    let n = n.get();
    let chain = primary.into_iter().chain(secondary);
    let batches = (1..=n)
        .map(|offset| chain.clone().skip(offset - 1).step_by(n).collect())
        .collect::<Vec<_>>();
    debug_assert_eq!(batches.len(), n);
    batches
}
