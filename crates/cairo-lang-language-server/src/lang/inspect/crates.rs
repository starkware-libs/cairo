use cairo_lang_filesystem::db::FilesGroup;
use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use crate::lang::db::AnalysisDatabase;
use crate::project::Crate;

/// Generates a Markdown text describing all crates in the database.
pub fn inspect_analyzed_crates(db: &AnalysisDatabase) -> String {
    let list = db
        .crates()
        .into_iter()
        .flat_map(|crate_id| Crate::reconstruct(db, crate_id))
        .sorted_by_key(|cr| cr.name.clone())
        .map(inspect_crate)
        .collect::<Vec<_>>()
        .join("");
    format!("# Analyzed Crates\n\n{list}")
}

/// Generates a Markdown fragment describing a single crate.
fn inspect_crate(cr: Crate) -> String {
    formatdoc! {
        r#"
        - `{name}`: `{source_path}`
            ```rust
            {settings}
            ```
        "#,
        name = cr.name,
        source_path = cr.source_path().display(),
        settings = indent_by(4, format!("{:#?}", cr.settings)),
    }
}
