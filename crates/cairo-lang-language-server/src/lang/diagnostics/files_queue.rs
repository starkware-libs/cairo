use std::collections::{HashMap, HashSet};

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileLongId;
use cairo_lang_utils::LookupIntern;
use lsp_types::Url;
use tracing::trace_span;

use super::file::FileBothFormats;
use super::state::FileDiagnostics;
use crate::lang::db::AnalysisDatabase;
use crate::lang::lsp::LsProtoGroup;

pub struct FilesQueue(Vec<FileBothFormats>);

impl IntoIterator for FilesQueue {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = FileBothFormats;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FilesQueue {
    pub fn new(db: &AnalysisDatabase, open_files: HashSet<Url>) -> Self {
        let mut rest_of_files = HashSet::new();
        let mut open_files = trace_span!("get_open_files_ids").in_scope(|| {
            open_files
                .into_iter()
                .filter_map(|url| db.file_for_url(&url).map(|file| FileBothFormats::new(file, url)))
                .collect::<Vec<_>>()
        });

        for crate_id in db.crates() {
            for module_id in db.crate_modules(crate_id).iter() {
                if let Ok(module_files) = db.module_files(*module_id) {
                    let unprocessed_files = module_files
                        .iter()
                        .copied()
                        .filter(|file_id| {
                            matches!(file_id.lookup_intern(db), FileLongId::OnDisk(_))
                        })
                        .map(|file| FileBothFormats::new(file, db.url_for_file(file)));

                    rest_of_files.extend(unprocessed_files);
                }
            }
        }

        // Remove open files from rest of files.
        for file in &open_files {
            rest_of_files.remove(file);
        }

        // Important: keep open files first so workers execute them at first too.
        open_files.extend(rest_of_files);

        Self(open_files)
    }

    pub fn urls(&self) -> impl Iterator<Item = Url> + '_ {
        self.0.iter().map(|file| file.url.clone())
    }

    pub fn worker_files_partition(&self, worker: usize, jobs_number: usize) -> Self {
        Self(
            self.0
                .iter()
                .enumerate()
                .filter(move |(i, _file)| i % jobs_number == worker)
                .map(|(_, file)| file)
                .cloned()
                .collect(),
        )
    }

    pub fn previous_generation_file_diagnostics(
        &self,
        diagnostics_for_file: &HashMap<Url, FileDiagnostics>,
    ) -> HashMap<Url, FileDiagnostics> {
        self.0
            .iter()
            .filter_map(|file| {
                diagnostics_for_file.get(&file.url).map(|diags| (file.url.clone(), diags.clone()))
            })
            .collect()
    }
}
