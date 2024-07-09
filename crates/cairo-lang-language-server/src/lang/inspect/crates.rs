use cairo_lang_filesystem::db::FilesGroup;
use indoc::formatdoc;
use itertools::Itertools;

use crate::lang::db::AnalysisDatabase;
use crate::markdown::fenced_code_block_lang;
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
    formatdoc! {r#"
        # Analyzed Crates

        {list}

    "#}
}

fn inspect_crate(cr: Crate) -> String {
    formatdoc! {
        r#"
        - `{name}`: `{source_path}`
        {settings}
        "#,
        name = cr.name,
        source_path = cr.source_path().display(),
        settings = indent(fenced_code_block_lang("rust", &format!("{:#?}", cr.settings))),
    }
}

fn indent(text: String) -> String {
    text.lines().map(|line| format!("    {}", line)).join("\n")
}
