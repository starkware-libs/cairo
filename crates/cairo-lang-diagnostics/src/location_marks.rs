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
    get_starting_location_marks(db, location, false)
}

pub fn get_starting_location_marks(
    db: &dyn cairo_lang_filesystem::db::FilesGroup,
    location: &DiagnosticLocation,
    is_multi_line_diagnostics: bool,
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
    let mut res = String::new();
    for _ in 0..col {
        res.push(' ');
    }
    match is_multi_line_diagnostics {
        true => res.push('V'),
        false => res.push('^'),
    }

    let subspan_in_first_line =
        TextSpan { start: span.start, end: std::cmp::min(first_line_end, span.end) };
    let marker_length = subspan_in_first_line.n_chars(&content);
    if marker_length > 1 {
        for _ in 0..marker_length - 2 {
            res.push('*');
        }
        match is_multi_line_diagnostics {
            true => res.push('*'),
            false => res.push('^'),
        }
    }
    match is_multi_line_diagnostics {
        true => {
            res.push('\n');
            res += first_line_span.take(&content);
        }
        false => res = first_line_span.take(&content).to_string() + "\n" + &res,
    }

    res
}

pub fn get_end_location_marks(
    db: &dyn cairo_lang_filesystem::db::FilesGroup,
    location: &DiagnosticLocation,
) -> String {
    // TODO(ilya, 10/10/2023): Handle locations which spread over a few lines.
    let content = db.file_content(location.file_id).expect("File missing from DB.");
    let summary = db.file_summary(location.file_id).expect("File missing from DB.");

    let span = &location.span;

    let TextPosition { line: last_line_idx, col: _ } =
        span.end.position_in_file(db, location.file_id).expect("Failed to find location in file.");
    let last_line_start = summary.line_offsets[last_line_idx];
    let last_line_end = match summary.line_offsets.get(last_line_idx + 1) {
        Some(offset) => offset.sub_width(TextWidth::from_char('\n')),
        None => summary.last_offset,
    };

    let last_line_span = TextSpan { start: last_line_start, end: last_line_end };
    let mut res = last_line_span.take(&content).to_string();
    let first_line_chars =
        last_line_span.take(&content).to_string().chars().clone().collect::<Vec<_>>();
    res.push('\n');
    let mut is_suffix = true;
    for c in first_line_chars.clone() {
        is_suffix &= c == ' ';
        match is_suffix {
            true => res.push(' '),
            false => res.push('*'),
        }
    }
    res.pop();
    res.push('^');

    res
}
