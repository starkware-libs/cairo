use std::sync::Arc;

use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::{define_short_id, Intern, LookupIntern, Upcast};
use itertools::Itertools;
use scarb_metadata::Metadata;
use tracing::{error, warn};

use crate::project::db::ProjectsContext;
use crate::project::digests::{Digestible, LsDigestsGroup};
use crate::project::project_manifest_path::ProjectManifestPath;
use crate::project::unmanaged_core_crate::LsUnmanagedCoreGroup;
use crate::project::{cairo_project, scarb, Crate};

define_short_id!(
    ProjectId,
    ProjectManifestPath,
    LsProjectsGroup,
    lookup_intern_project,
    intern_project
);

// FIXME(mkaput): Scarb projects may be part of a workspace,
//   that is the canonical source of project information and `scarb metadata`
//   always returns information for the workspace.
//   This has to be supported by bookkeeping a concept of "canonical" ProjectId for given ProjectId.
/// Main query group for the projects' database.
#[salsa::query_group(LsProjectsDatabase)]
pub trait LsProjectsGroup:
    LsDigestsGroup + Upcast<dyn LsDigestsGroup> + LsUnmanagedCoreGroup + ProjectsContext
{
    #[salsa::interned]
    fn intern_project(&self, project: ProjectManifestPath) -> ProjectId;

    /// List of projects that are currently open in the language client.
    ///
    /// This list is highly dynamic and must be maintained externally to the query system.
    #[salsa::input]
    fn open_projects(&self) -> Arc<OrderedHashSet<ProjectId>>;

    /// Compute the current list of crates to analyze.
    ///
    /// This is the final query of the project model database.
    fn crates(&self) -> Arc<[Arc<Crate>]>;

    /// Compute the list of crates used in the given project.
    fn project_crates(&self, project: ProjectId) -> Arc<[Arc<Crate>]>;

    // TODO(mkaput): Make this an input that is asynchronously populated from background worker.
    /// Calls `scarb metadata` for the given project.
    #[salsa::invoke(scarb::scarb_metadata)]
    fn scarb_metadata(&self, project: ProjectId) -> Option<Arc<Metadata>>;
}

fn crates(db: &dyn LsProjectsGroup) -> Arc<[Arc<Crate>]> {
    let mut crates = db
        .open_projects()
        .iter()
        .flat_map(|project| db.project_crates(*project).to_vec())
        .collect::<Vec<_>>();

    // If the `core` crate is missing in the list, add the unmanaged one.
    // This way, at least some level of analysis will be available for heavily incomplete projects,
    // or for detached files.
    if !crates.iter().any(|cr| cr.is_core()) {
        if let Some(core) = db.unmanaged_core() {
            crates.push(core);
        } else {
            // TODO(mkaput): Send a notification to the language client about the error.
            error!(
                "no project provided the `core` crate and could not find an unmanaged one, \
                 language server will likely start panicking soon"
            );
        }
    }

    // Cairo does not support compilation units where two crates with the same name exist.
    // In such cases, the latter crate initialized will overwrite the former one.
    // This may lead to surprising analysis for the user, so they should be warned about it.
    let duplicates = crates.iter().map(|cr| &cr.name).duplicates().sorted().collect::<Vec<_>>();
    if !duplicates.is_empty() {
        // TODO(mkaput): Send a notification to the language client about the error.
        warn!(
            "opened projects define duplicate crates with the same name, this may lead to \
             surprising code analysis behaviour: {duplicates:?}"
        );
    }

    crates.into()
}

#[tracing::instrument(level = "trace", skip_all, fields(project = %project.lookup_intern(db)))]
fn project_crates(db: &dyn LsProjectsGroup, project: ProjectId) -> Arc<[Arc<Crate>]> {
    let manifest_path = project.lookup_intern(db);

    db.digest(Digestible::from(&manifest_path).intern(db));

    match manifest_path {
        ProjectManifestPath::CairoProject(_) => cairo_project::project_crates(db, project),
        ProjectManifestPath::Scarb(_) => scarb::project_crates(db, project),
    }
}
