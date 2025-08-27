const EXPOSE_MACRO_PREFIX: &str = "expose!";
const EXPOSE_MACRO_NAME: &str = "expose";

/// Process expose!() calls in the expanded text, returning the processed content and whether any
/// expose calls were found.
pub fn process_expose_calls_in_expansion(text: &str) -> (String, bool) {
    let mut result = text.to_string();
    let mut has_expose = false;
    for pattern in [ExposePattern::Braces, ExposePattern::Parentheses, ExposePattern::Brackets] {
        if let Some(replacement) = extract_and_replace_expose_pattern(&result, pattern) {
            result = replacement;
            has_expose = true;
        }
    }

    (result, has_expose)
}

/// Check if a macro name matches the expose macro name.
pub fn is_expose_macro(macro_name: &str) -> bool {
    macro_name == EXPOSE_MACRO_NAME
}

/// Represents the different bracket types that expose!() can use.
#[derive(Clone, Copy)]
enum ExposePattern {
    Braces,
    Parentheses,
    Brackets,
}

impl ExposePattern {
    /// Returns the opening delimiter for this pattern.
    fn opening_delimiter(self) -> char {
        match self {
            ExposePattern::Braces => '{',
            ExposePattern::Parentheses => '(',
            ExposePattern::Brackets => '[',
        }
    }

    /// Returns the closing delimiter for this pattern.
    fn closing_delimiter(self) -> char {
        match self {
            ExposePattern::Braces => '}',
            ExposePattern::Parentheses => ')',
            ExposePattern::Brackets => ']',
        }
    }

    /// Returns the full pattern string for searching.
    fn pattern_string(self) -> String {
        format!("{}{}", EXPOSE_MACRO_PREFIX, self.opening_delimiter())
    }
}

/// Extracts and replaces the first occurrence of the given expose pattern in the text.
fn extract_and_replace_expose_pattern(text: &str, pattern: ExposePattern) -> Option<String> {
    let pattern_str = pattern.pattern_string();
    let start_pos = text.find(&pattern_str)?;
    let content_start = start_pos + pattern_str.len();
    let content_end = find_matching_delimiter(
        text,
        content_start,
        pattern.opening_delimiter(),
        pattern.closing_delimiter(),
    )?;
    let inner_content = &text[content_start..content_end];
    let result = format!("{}{}{}", &text[..start_pos], inner_content, &text[content_end + 1..]);
    Some(result)
}

/// Finds the position of the matching closing delimiter, handling nested delimiters.
fn find_matching_delimiter(
    text: &str,
    start_pos: usize,
    open_char: char,
    close_char: char,
) -> Option<usize> {
    let chars: Vec<char> = text.chars().collect();
    let mut depth = 1;
    for (i, &ch) in chars.iter().enumerate().skip(start_pos) {
        match ch {
            ch if ch == open_char => depth += 1,
            ch if ch == close_char => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}
