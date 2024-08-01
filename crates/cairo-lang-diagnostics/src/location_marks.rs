use std::fmt::Write;

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

    let mut span = location.span;
    // Make sure that the span doesn't end after the end of the file.
    span.end = std::cmp::min(summary.last_offset, span.end);

    let TextPosition { line: first_line_idx, col: start_col } = span
        .start
        .position_in_file(db, location.file_id)
        .expect("Failed to find location in file.");

    let TextPosition { line: last_line_idx, col: _ } =
        span.end.position_in_file(db, location.file_id).expect("Failed to find location in file.");

    if first_line_idx == last_line_idx {
        let first_line_start = summary.line_offsets[first_line_idx];
        let first_line_end = match summary.line_offsets.get(first_line_idx + 1) {
            Some(offset) => offset.sub_width(TextWidth::from_char('\n')),
            None => summary.last_offset,
        };

        let first_line_span = TextSpan { start: first_line_start, end: first_line_end };
        let mut res = first_line_span.take(&content).to_string();
        res.push('\n');
        for _ in 0..start_col {
            res.push(' ');
        }
        res.push('^');

        let subspan_in_first_line =
            TextSpan { start: span.start, end: std::cmp::min(first_line_end, span.end) };
        let marker_length = subspan_in_first_line.n_chars(&content);
        if marker_length > 1 {
            for _ in 0..marker_length - 2 {
                res.push('*');
            }
            res.push('^');
        }

        res
    } else {
        let mut res = String::new();
        // Number of indentations
        let mut tab = 0;
        let mut prefix = "\\   ";
        let mut lines = span.take(&content).split('\n').peekable();
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
                tab -= 1
            }
            res.write_str(&format!("{prefix}{}{}\n", "    ".repeat(tab), line)).unwrap();
            // If we open a scope that is not closed on the same line.
            // If it's an else we need to indent for the inside of the else
            if line.contains('{') && !line.contains('}') || is_else {
                tab += 1
            }
            prefix = "|   ";
            // If it's the last line point to where the span ends.
            if lines.peek().is_none() {
                res.write_str(&format!("|___{}^", "_".repeat(std::cmp::max(line.len(), 1) - 1)))
                    .unwrap();
            }
        }
        res
    }
}
