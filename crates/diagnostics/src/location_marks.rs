use filesystem::span::TextOffset;

use crate::DiagnosticLocation;

#[cfg(test)]
#[path = "location_marks_test.rs"]
mod test;

#[allow(dead_code)]
pub fn get_location_marks(
    db: &dyn filesystem::db::FilesGroup,
    location: DiagnosticLocation,
) -> String {
    let content = db.file_content(location.file_id).expect("File missing from DB.");
    let summary = db.file_summary(location.file_id).expect("File missing from DB.");

    let span = &location.span;
    let first_line_idx = span
        .start
        .position_in_file(db, location.file_id)
        .expect("Failed to find location in file.")
        .line;
    let first_line_start = summary.line_offsets[first_line_idx].0;
    let first_line_end = match summary.line_offsets.get(first_line_idx + 1) {
        Some(TextOffset(offset)) => offset - 1,
        None => summary.total_length,
    };

    let first_line = &content[first_line_start..first_line_end];

    let mut res = first_line.to_string();
    res.push('\n');
    for _ in first_line_start..span.start.0 {
        res.push(' ');
    }
    res.push('^');

    let marker_length = std::cmp::min(first_line_end, span.end.0) - span.start.0;
    if marker_length > 2 {
        for _ in 0..marker_length - 2 {
            res.push('*');
        }
        res.push('^');
    }

    res
}
