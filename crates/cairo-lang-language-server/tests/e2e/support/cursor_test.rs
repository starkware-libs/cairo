use lsp_types::Position;

use super::cursors;

#[test]
fn test_cursors() {
    let (text, cursors) = cursors(
        r#"<caret>
He<caret>llo, <caret>world!
"#,
    );
    assert_eq!(text.find('<'), None);
    assert_eq!(cursors.caret(0), Position::new(0, 0));
    assert_eq!(cursors.caret(1), Position::new(1, 2));
    assert_eq!(cursors.caret(2), Position::new(1, 7));
}
