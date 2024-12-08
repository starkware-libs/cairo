use cairo_lang_filesystem::span::{TextPosition, TextSpan, TextWidth};

use crate::DiagnosticLocation;

#[cfg(test)]
#[path = "location_marks_test.rs"]
mod test;

pub fn get_location_marks(
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
    for _ in 0..col {
        res.push(' ');
    }
    res.push('^');

    let subspan_in_first_line =
        TextSpan { start: span.start, end: std::cmp::min(first_line_end, span.end) };
    let marker_length = subspan_in_first_line.n_chars(&content);
    if marker_length > 1 {
        for _ in 0..marker_length - 1 {
            res.push('^');
        }
    }

    res
}

pub fn get_multiple_lines_location_marks(
    db: &dyn cairo_lang_filesystem::db::FilesGroup,
    location: &DiagnosticLocation,
) -> String {
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
    let mut res = "  ".to_string() + first_line_span.take(&content) + "\n _";
    for _ in 0..col {
        res.push('_');
    }
    res += "^\n";

    let TextPosition { line: last_line_idx, col: _ } =
        span.end.position_in_file(db, location.file_id).expect("Failed to find location in file.");

    let mut last_line_len = 0;
    for row_index in first_line_idx + 1..=last_line_idx {
        let line_start = summary.line_offsets[row_index];
        let line_end = match summary.line_offsets.get(row_index + 1) {
            Some(offset) => offset.sub_width(TextWidth::from_char('\n')),
            None => summary.last_offset,
        };

        let line_span = TextSpan { start: line_start, end: line_end };
        res += "| ";
        res += line_span.take(&content);
        last_line_len = line_span.n_chars(&content);
        res.push('\n');
    }
    res += "|";
    for _ in 0..last_line_len {
        res.push('_');
    }
    res.push('^');

    res
}
