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

use crate::config::Config;
use crate::lang::db::AnalysisDatabase;
use crate::lang::lsp::LsProtoGroup;
use crate::server::client::Notifier;
use crate::toolchain::scarb::ScarbToolchain;
use crate::{Backend, Tricks, env_config};

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
    scarb_toolchain: ScarbToolchain,
}

impl AnalysisDatabaseSwapper {
    /// Creates a new `AnalysisDatabaseSwapper`.
    pub fn new(scarb_toolchain: ScarbToolchain) -> Self {
        Self {
            last_replace: SystemTime::now(),
            db_replace_interval: env_config::db_replace_interval(),
            scarb_toolchain,
        }
    }

    /// Checks if enough time has passed since last db swap, and if so, swaps the database.
    pub fn maybe_swap(
        &mut self,
        db: &mut AnalysisDatabase,
        open_files: &HashSet<Url>,
        config: &Config,
        tricks: &Tricks,
        notifier: &Notifier,
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

        self.swap(db, open_files, config, tricks, notifier)
    }

    /// Swaps the database.
    #[tracing::instrument(skip_all)]
    fn swap(
        &mut self,
        db: &mut AnalysisDatabase,
        open_files: &HashSet<Url>,
        config: &Config,
        tricks: &Tricks,
        notifier: &Notifier,
    ) {
        let Ok(new_db) = catch_unwind(AssertUnwindSafe(|| {
            let mut new_db = AnalysisDatabase::new(tricks);
            self.migrate_file_overrides(&mut new_db, db, open_files);
            self.detect_crates_for_open_files(&mut new_db, open_files, config, notifier);
            new_db
        })) else {
            error!("caught panic when preparing new db for swap");
            return;
        };

        *db = new_db;

        self.last_replace = SystemTime::now();
    }

    /// Makes sure that all open files exist in the new db, with their current changes.
    fn migrate_file_overrides(
        &self,
        new_db: &mut AnalysisDatabase,
        old_db: &AnalysisDatabase,
        open_files: &HashSet<Url>,
    ) {
        let overrides = old_db.file_overrides();
        let mut new_overrides: OrderedHashMap<FileId, Arc<str>> = Default::default();
        for uri in open_files {
            let Some(file_id) = old_db.file_for_url(uri) else {
                // This branch is hit for open files that have never been seen by the old db.
                // This is a strange condition, but it is OK to just not think about such files
                // here.
                continue;
            };
            let new_file_id = file_id.lookup_intern(old_db).intern(new_db);
            if let Some(content) = overrides.get(&file_id) {
                new_overrides.insert(new_file_id, content.clone());
            }
        }
        new_db.set_file_overrides(Arc::new(new_overrides));
    }

    /// Ensures all open files have their crates detected to regenerate crates state.
    fn detect_crates_for_open_files(
        &self,
        new_db: &mut AnalysisDatabase,
        open_files: &HashSet<Url>,
        config: &Config,
        notifier: &Notifier,
    ) {
        for uri in open_files {
            let Ok(file_path) = uri.to_file_path() else {
                // We are only interested in physical files.
                continue;
            };

            Backend::detect_crate_for(new_db, &self.scarb_toolchain, config, &file_path, notifier);
        }
    }
}
