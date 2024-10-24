use std::collections::HashSet;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::Arc;
use std::time::{Duration, SystemTime};

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern};
use lsp_types::Url;
use tracing::{error, warn};

use crate::lang::db::AnalysisDatabase;
use crate::lang::lsp::LsProtoGroup;
use crate::{Tricks, env_config};

/// Swaps entire [`AnalysisDatabase`] with empty one periodically.
///
/// Salsa does not perform GC, which means that whatever computes in query groups stays in memory
/// forever (with little exception being the LRU mechanism).
/// The usage patterns of Salsa queries in the Cairo compiler cause Salsa to steadily allocate
/// new memory, which is a problem if the user is having a long coding session.
/// This object realises a nuclear GC strategy by wiping the entire analysis database from time to
/// time.
///
/// The swapping period can be configured with environment variables.
/// Consult [`env_config::db_replace_interval`] for more information.
///
/// The new database has a clean state.
/// It is expected that diagnostics will be refreshed on it as quickly as possible, otherwise
/// the entire workspace would be recompiled at an undetermined time leading to bad UX delays.
pub struct AnalysisDatabaseSwapper {
    last_replace: SystemTime,
    db_replace_interval: Duration,
}

impl AnalysisDatabaseSwapper {
    /// Creates a new `AnalysisDatabaseSwapper`.
    pub fn new() -> Self {
        Self {
            last_replace: SystemTime::now(),
            db_replace_interval: env_config::db_replace_interval(),
        }
    }

    /// Checks if enough time has passed since last db swap, and if so, swaps the database.
    pub fn maybe_swap(
        &mut self,
        db: &mut AnalysisDatabase,
        open_files: &HashSet<Url>,
        tricks: &Tricks,
    ) {
        let Ok(elapsed) = self.last_replace.elapsed() else {
            warn!("system time went backwards, skipping db swap");

            // Reset last replace time because in this place the old value will never make sense.
            self.last_replace = SystemTime::now();

            return;
        };

        if elapsed <= self.db_replace_interval {
            // Not enough time passed since the last swap.
            return;
        }

        self.swap(db, open_files, tricks)
    }

    /// Swaps the database.
    #[tracing::instrument(skip_all)]
    fn swap(&mut self, db: &mut AnalysisDatabase, open_files: &HashSet<Url>, tricks: &Tricks) {
        let Ok(new_db) = catch_unwind(AssertUnwindSafe(|| {
            let mut new_db = AnalysisDatabase::new(tricks);
            ensure_exists_in_db(&mut new_db, db, open_files.iter());
            new_db
        })) else {
            error!("caught panic when preparing new db for swap");
            return;
        };

        *db = new_db;

        self.last_replace = SystemTime::now();
    }
}

/// Makes sure that all open files exist in the new db, with their current changes.
fn ensure_exists_in_db<'a>(
    new_db: &mut AnalysisDatabase,
    old_db: &AnalysisDatabase,
    open_files: impl Iterator<Item = &'a Url>,
) {
    let overrides = old_db.file_overrides();
    let mut new_overrides: OrderedHashMap<FileId, Arc<str>> = Default::default();
    for uri in open_files {
        let Some(file_id) = old_db.file_for_url(uri) else {
            // This branch is hit for open files that have never been seen by the old db.
            // This is a strange condition, but it is OK to just not think about such files here.
            continue;
        };
        let new_file_id = file_id.lookup_intern(old_db).intern(new_db);
        if let Some(content) = overrides.get(&file_id) {
            new_overrides.insert(new_file_id, content.clone());
        }
    }
    new_db.set_file_overrides(Arc::new(new_overrides));
}
