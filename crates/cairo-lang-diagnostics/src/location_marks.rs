use std::sync::Arc;

use cairo_lang_filesystem::span::{FileSummary, TextPosition, TextSpan, TextWidth};
use itertools::repeat_n;

use crate::DiagnosticLocation;

#[cfg(test)]
#[path = "location_marks_test.rs"]
mod test;

/// Given a diagnostic location, returns a string with the location marks.
pub fn get_location_marks(
    db: &dyn cairo_lang_filesystem::db::FilesGroup,
    location: &DiagnosticLocation,
    skip_middle_lines: bool,
) -> String {
    let span = &location.span;
    let summary = db.file_summary(location.file_id).expect("File missing from DB.");
    let TextPosition { line: first_line_idx, col: _ } = span
        .start
        .position_in_file(db, location.file_id)
        .expect("Failed to find location in file.");
    if span.end <= summary.last_offset {
        let TextPosition { line: last_line_idx, col: _ } = span
            .end
            .position_in_file(db, location.file_id)
            .expect("Failed to find location in file.");
        if first_line_idx != last_line_idx {
            return get_multiple_lines_location_marks(db, location, skip_middle_lines);
        }
    }
    get_single_line_location_marks(db, location)
}

/// Given a single line diagnostic location, returns a string with the location marks.
fn get_single_line_location_marks(
    db: &dyn cairo_lang_filesystem::db::FilesGroup,
    location: &DiagnosticLocation,
) -> String {
    // TODO(ilya, 10/10/2023): Handle locations which spread over a few lines.
    let content = db.file_content(location.file_id).expect("File missing from DB.");
    let summary = db.file_summary(location.file_id).expect("File missing from DB.");
    let span = &location.span;
    let TextPosition { line: first_line_idx, col } = span
        .start
        .position_in_file(db, location.file_id)
        .expect("Failed to find location in file.");
    let first_line_start = summary.line_offsets[first_line_idx];
    let first_line_end = match summary.line_offsets.get(first_line_idx + 1) {
        Some(offset) => offset.sub_width(TextWidth::from_char('\n')),
        None => summary.last_offset,
    };

    let first_line_span = TextSpan { start: first_line_start, end: first_line_end };
    let mut res = first_line_span.take(&content).to_string();
    res.push('\n');
    res.extend(repeat_n(' ', col));
    let subspan_in_first_line =
        TextSpan { start: span.start, end: std::cmp::min(first_line_end, span.end) };
    let marker_length = subspan_in_first_line.n_chars(&content);
    // marker_length can be 0 if the span is empty.
    res.extend(repeat_n('^', std::cmp::max(marker_length, 1)));

    res
}

/// Given a multiple lines diagnostic location, returns a string with the location marks.
fn get_multiple_lines_location_marks(
    db: &dyn cairo_lang_filesystem::db::FilesGroup,
    location: &DiagnosticLocation,
    skip_middle_lines: bool,
) -> String {
    let content = db.file_content(location.file_id).expect("File missing from DB.");
    let summary = db.file_summary(location.file_id).expect("File missing from DB.");

    let span = &location.span;
    let TextPosition { line: first_line_idx, col } = span
        .start
        .position_in_file(db, location.file_id)
        .expect("Failed to find location in file.");
    let mut res = get_line_content(summary.clone(), first_line_idx, content.clone(), true);
    res += " _";
    res.extend(repeat_n('_', col));
    res += "^\n";
    let TextPosition { line: last_line_idx, col: end_line_col } =
        span.end.position_in_file(db, location.file_id).expect("Failed to find location in file.");

    const LINES_TO_REPLACE_MIDDLE: usize = 3;
    if !skip_middle_lines || first_line_idx + LINES_TO_REPLACE_MIDDLE > last_line_idx {
        for row_index in first_line_idx + 1..=last_line_idx - 1 {
            res += &get_line_content(summary.clone(), row_index, content.clone(), false);
        }
    } else {
        res += "| ...\n";
    }

    res += &get_line_content(summary.clone(), last_line_idx, content.clone(), false);
    res += "|";
    res.extend(repeat_n('_', end_line_col));
    res.push('^');

    res
}

fn get_line_content(
    summary: Arc<FileSummary>,
    row_index: usize,
    content: Arc<str>,
    first_line: bool,
) -> String {
    let line_start = summary.line_offsets[row_index];
    let line_end = match summary.line_offsets.get(row_index + 1) {
        Some(offset) => offset.sub_width(TextWidth::from_char('\n')),
        None => summary.last_offset,
    };

    let line_span = TextSpan { start: line_start, end: line_end };
    format!("{}{}\n", if first_line { "  " } else { "| " }, line_span.take(&content))
}
