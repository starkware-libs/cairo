use std::collections::HashMap;
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
///
/// A cursor can be identified by either an index or a unique name.
pub fn cursors(text: &str) -> (String, Cursors) {
    // Trim the whitespace, because usually there is one for code clarity.
    let text = text.trim();

    let mut cursors = Cursors::new();
    let mut output_text = String::with_capacity(text.len());
    let mut position = Position::new(0, 0);

    // Peek multiple characters at once, check if they match the needle, and reset the peek.
    let peek = |it: &mut MultiPeek<Chars>, needle: &str| -> bool {
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
    };

    // Consume multiple characters at once, asserting they match the needle.
    let eat = |it: &mut MultiPeek<Chars>, needle: &str| {
        for needle_ch in needle.chars() {
            let haystack_ch = it.next();
            assert_eq!(haystack_ch, Some(needle_ch));
        }
    };

    // Take characters until the terminator is found. Terminator is not consumed.
    let take_until = |it: &mut MultiPeek<Chars>, terminator: char| -> String {
        let mut result = String::new();
        while let Some(ch) = it.peek() {
            if *ch == terminator {
                break;
            }
            result.push(it.next().unwrap());
        }
        result
    };

    let mut it = text.chars().multipeek();
    while let Some(ch) = it.next() {
        match ch {
            // Handle the '<caret>' marker.
            '<' if peek(&mut it, "caret") => {
                eat(&mut it, "caret");
                match it.next() {
                    Some('>') => {
                        cursors.add(position, None);
                    }
                    Some('=') => {
                        let name = take_until(&mut it, '>');
                        eat(&mut it, ">");
                        cursors.add(position, Some(name));
                    }
                    _ => panic!("expected '>' or '=' after '<caret'"),
                }
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

    (output_text, cursors)
}

// TODO(mkaput): Implement selections when we'll need them.
/// A collection of cursors in a text document.
///
/// See [`cursors`] docs for more information.
pub struct Cursors {
    cursors: Vec<Position>,
    names: HashMap<String, usize>,
}

impl Cursors {
    fn new() -> Self {
        Self { cursors: Vec::new(), names: HashMap::new() }
    }

    fn add(&mut self, cursor: Position, name: Option<String>) {
        let idx = self.cursors.len();
        self.cursors.push(cursor);
        if let Some(name) = name {
            self.names.insert(name, idx);
        }
    }

    fn name_to_idx(&self, name: &str) -> usize {
        *self.names.get(name).unwrap_or_else(|| panic!("named cursor not found: {name}"))
    }

    /// Get specified caret.
    pub fn caret(&self, id: impl CursorId) -> Position {
        let idx = id.as_idx(self);
        *self.cursors.get(idx).unwrap_or_else(|| panic!("cursor not found: {idx}"))
    }

    /// Get all carets that have a name, sorted by name.
    pub fn named_carets(&self) -> Vec<(String, Position)> {
        self.names
            .iter()
            .sorted_by_key(|(name, _)| (*name).clone())
            .map(|(name, &idx)| (name.clone(), self.caret(idx)))
            .collect()
    }
}

/// A way of identifying a cursor, either an index in the text, or a unique name.
pub trait CursorId {
    /// Get the index of the cursor in the text using private data of [`Cursors`].
    fn as_idx(&self, cursors: &Cursors) -> usize;
}

impl CursorId for usize {
    fn as_idx(&self, _: &Cursors) -> usize {
        *self
    }
}

impl CursorId for &'_ str {
    fn as_idx(&self, cursors: &Cursors) -> usize {
        cursors.name_to_idx(self)
    }
}
