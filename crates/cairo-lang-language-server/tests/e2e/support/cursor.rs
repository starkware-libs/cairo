use std::cmp::min;
use std::str::Chars;

use itertools::{Itertools, MultiPeek};
use lsp_types::{Position, Range};

#[path = "cursor_test.rs"]
mod test;

/// Extracts cursor markers from the text.
///
/// A cursor is a marker in the text that can be one of the following:
/// 1. `<caret>` specifies the position where the caret should be placed.
/// 2. `<selection>` and `</selection>` specify the start and end of the selected text range.
pub fn cursors(text: &str) -> (String, Cursors) {
    // Trim the whitespace, because usually there is one for code clarity.
    let text = text.trim();

    let mut cursors = Cursors::new();
    let mut output_text = String::with_capacity(text.len());
    let mut position = Position::new(0, 0);

    let mut it = text.chars().multipeek();
    while let Some(ch) = it.next() {
        match ch {
            // Handle the '<caret>' marker.
            '<' if peek(&mut it, "caret") => {
                eat(&mut it, "caret>");
                cursors.add(position);
            }
            _ => {
                // Add the character to the output text.
                output_text.push(ch);

                // Increment line and character.
                if ch == '\n' {
                    position.line += 1;
                    position.character = 0;
                } else {
                    position.character += 1;
                }
            }
        }
    }

    return (output_text, cursors);

    /// Peek multiple characters at once, check if they match the needle, and reset the peek.
    fn peek(it: &mut MultiPeek<Chars<'_>>, needle: &str) -> bool {
        let mut matched = true;
        for needle_ch in needle.chars() {
            let Some(&haystack_ch) = it.peek() else {
                matched = false;
                break;
            };
            if needle_ch != haystack_ch {
                matched = false;
                break;
            }
        }
        it.reset_peek();
        matched
    }

    /// Consume multiple characters at once, asserting they match the needle.
    fn eat(it: &mut MultiPeek<Chars<'_>>, needle: &str) {
        for needle_ch in needle.chars() {
            let haystack_ch = it.next();
            assert_eq!(haystack_ch, Some(needle_ch));
        }
    }
}

// TODO(mkaput): Implement selections when we'll need them.
/// A collection of cursors in a text document.
///
/// See [`cursors`] docs for more information.
pub struct Cursors {
    cursors: Vec<Position>,
}

impl Cursors {
    fn new() -> Self {
        Self { cursors: Vec::new() }
    }

    fn add(&mut self, cursor: Position) {
        self.cursors.push(cursor);
    }

    /// Get specified caret.
    pub fn caret(&self, idx: usize) -> Position {
        *self.cursors.get(idx).unwrap_or_else(|| panic!("cursor not found: {idx}"))
    }

    /// Get all carets.
    pub fn carets(&self) -> Vec<Position> {
        self.cursors.clone()
    }
}

/// Creates a snippet (lines of interest) of the source text showing a caret at the specified
/// position.
pub fn peek_caret(text: &str, position: Position) -> String {
    let mut snippet = text.to_owned();
    snippet.insert_str(index_in_text(text, position), "<caret>");
    snippet.lines().nth(position.line as usize).unwrap().to_owned() + "\n"
}

/// Creates a snippet (lines of interest) of the source text showing a selection of the specified
/// range.
pub fn peek_selection(text: &str, range: &Range) -> String {
    let mut snippet = text.to_owned();
    assert!(range.start <= range.end);
    snippet.insert_str(index_in_text(text, range.start), "<sel>");
    snippet.insert_str(index_in_text(text, range.end) + "<sel>".len(), "</sel>");
    snippet
        .lines()
        .skip(range.start.line as usize)
        .take(range.end.line as usize - range.start.line as usize + 1)
        .join("\n")
        + "\n"
}

/// Converts a [`Position`] to a char-bounded index in the text.
///
/// This function assumes UTF-8 position encoding.
fn index_in_text(text: &str, position: Position) -> usize {
    let mut offset = 0;
    let mut lines = text.lines();
    for line in lines.by_ref().take(position.line as usize) {
        offset += line.len() + "\n".len();
    }
    if let Some(line) = lines.next() {
        offset += min(position.character as usize, line.len());
    }
    offset
}
