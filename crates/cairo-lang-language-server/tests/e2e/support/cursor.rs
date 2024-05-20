use std::str::Chars;

use itertools::{Itertools, MultiPeek};
use tower_lsp::lsp_types::Position;

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
