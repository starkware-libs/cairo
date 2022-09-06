use crate::DiagnosticLocation;

#[cfg(test)]
#[path = "location_marks_test.rs"]
mod test;

pub fn get_location_marks(
    db: &dyn filesystem::db::FilesGroup,
    location: DiagnosticLocation,
) -> String {
    let content = db.file_content(location.file_id).unwrap();
    let summary = db.file_summary(location.file_id).unwrap();
    let first_line_idx = location.span.start.position_in_file(db, location.file_id).unwrap().line;
    let first_line_start = summary.line_offsets[first_line_idx].0;
    let first_line_end = summary.line_offsets[first_line_idx + 1].0;
    let first_line = &content[first_line_start..first_line_end];

    let mut res = first_line.to_string();
    for _ in first_line_start..location.span.start.0 {
        res.push(' ');
    }
    res.push('^');

    let span_length = std::cmp::min(
        first_line_end - location.span.start.0 - 1,
        location.span.end.0 - location.span.start.0,
    );

    if span_length > 2 {
        for _ in 0..span_length - 2 {
            res.push('*');
        }
        res.push('^');
    }

    return res;
}
