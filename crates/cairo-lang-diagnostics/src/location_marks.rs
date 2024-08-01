use cairo_lang_filesystem::span::{TextPosition, TextSpan, TextWidth};

use crate::DiagnosticLocation;

#[cfg(test)]
#[path = "location_marks_test.rs"]
mod test;

/// Get the code corresponding to a diagnostic location. The first line will always be complete even
/// if the span starts after the first col.
/// If the diagnostic is on a single line it will return something like this:
/// ```text
/// let val = function().other_function()
///                     ^**************^
/// ```
/// If it's a multiline diagnostic it will return something like this:
/// ```text
/// \   let val = function
/// |   .other_function()
/// |___________________^
/// ```
pub fn get_location_marks(
    db: &dyn cairo_lang_filesystem::db::FilesGroup,
    location: &DiagnosticLocation,
) -> String {
    let (content, start_col, marker_length) = extract_content(db, location);
    generate_marks(&content, start_col, marker_length)
}

/// Gets the string from a diagnostic location. The first line is always complete even if the span
/// starts after.
fn extract_content(
    db: &dyn cairo_lang_filesystem::db::FilesGroup,
    location: &DiagnosticLocation,
) -> (String, usize, usize) {
    let content = db.file_content(location.file_id).expect("File missing from DB.");
    let summary = db.file_summary(location.file_id).expect("File missing from DB.");

    let mut span = location.span;
    // Ensure the span does not go beyond the end of the file.
    span.end = std::cmp::min(summary.last_offset, span.end);

    let TextPosition { line: first_line_idx, col: start_col } = span
        .start
        .position_in_file(db, location.file_id)
        .expect("Failed to find location in file.");

    let TextPosition { line: last_line_idx, col: _ } =
        span.end.position_in_file(db, location.file_id).expect("Failed to find location in file.");

    let first_line_start = summary.line_offsets[first_line_idx];
    if first_line_idx == last_line_idx {
        let first_line_end = match summary.line_offsets.get(first_line_idx + 1) {
            Some(offset) => offset.sub_width(TextWidth::from_char('\n')),
            None => summary.last_offset,
        };

        let first_line_span = TextSpan { start: first_line_start, end: first_line_end };
        let line_content = first_line_span.take(&content).to_string();
        let subspan_in_first_line =
            TextSpan { start: span.start, end: std::cmp::min(first_line_end, span.end) };
        let marker_length = subspan_in_first_line.n_chars(&content);
        (line_content, start_col, marker_length)
    } else {
        span.start = first_line_start;
        let span_content = span.take(&content).to_string();
        (span_content, 0, 0)
    }
}

/// Generate the diagnostic marks on a [&str]
///
/// # Arguments
///
/// * `snippet` - The code to add marks to.
/// * `start_col` - If the snippet is a 1 liner will start the mark at this index else will not use
///   it.
/// * `marker_length` - If the snippet is a 1 liner will use this as the marker length else will not
///   use it.
fn generate_marks(snippet: &str, start_col: usize, marker_length: usize) -> String {
    let mut res = String::new();

    // Single-line span handling
    if !snippet.contains('\n') {
        res.push_str(snippet);
        res.push('\n');
        for _ in 0..start_col {
            res.push(' ');
        }
        res.push('^');
        if marker_length > 1 {
            for _ in 0..marker_length - 2 {
                res.push('*');
            }
            res.push('^');
        }
        return res;
    }

    // Multi-line span handling
    let mut lines = snippet.split('\n').peekable();
    let mut tab = 0;
    let mut prefix = "\\   ";

    while let Some(line) = lines.next() {
        let line = line.trim();
        // Will check if we reached an `} else {` or an `} else {}` or `} else {};`
        let is_else = line.starts_with('}')
            && (line.ends_with('{') || line.ends_with("{}") || line.ends_with("{};"))
            && line.contains("else");

        if tab > 0
            // If we close a scope that wasn't opened in the same line.
            // Or if it's an else we need to deindent the line of the else.
            && (((line.ends_with('}') || line.ends_with("};") || line.ends_with("},"))
                && !line.contains('{'))
                || is_else)
        {
            tab -= 1;
        }

        res.push_str(&format!("{prefix}{}{}\n", "    ".repeat(tab), line));
        // If we open a scope that is not closed on the same line.
        // If it's an else we need to indent for the inside of the else

        if line.contains('{') && !line.contains('}') || is_else {
            tab += 1;
        }
        // If it's the last line point to where the span ends.
        prefix = "|   ";
        if lines.peek().is_none() {
            res.push_str(&format!("|___{}^", "_".repeat(std::cmp::max(line.len(), 1) - 1)));
        }
    }

    res
}
