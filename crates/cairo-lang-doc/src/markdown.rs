use std::borrow::Cow;

use itertools::Itertools;

#[cfg(test)]
#[path = "markdown_test.rs"]
mod test;

/// Applies a series of transformations to the given Markdown text taken from documentation
/// comments, making it suitable for general use for presentation.
///
/// The transformations include:
/// 1. Adding `cairo` language code to all fenced code blocks that do not specify language.
/// 2. Ensuring that the text ends with `\n`.
pub fn cleanup_doc_markdown(mut text: String) -> String {
    text = convert_fenced_code_blocks_to_cairo(text);
    text = ensure_trailing_newline(text);
    text
}

fn convert_fenced_code_blocks_to_cairo(mut text: String) -> String {
    let mut in_cairo_fence = false;
    text = text
        .lines()
        .map(|line| match (line.strip_prefix("```"), in_cairo_fence) {
            // Start of a fenced code block without language code.
            (Some(rest), false) if rest.trim_start().is_empty() => {
                in_cairo_fence = true;
                Cow::Owned(format!("```cairo{rest}"))
            }
            // Start of a fenced code block but with some language code.
            (Some(_), false) => {
                in_cairo_fence = true;
                Cow::Borrowed(line)
            }
            // End of a fenced code block.
            (Some(_), true) => {
                in_cairo_fence = false;
                Cow::Borrowed(line)
            }
            // Unrelated line.
            (None, _) => Cow::Borrowed(line),
        })
        .join("\n");
    text
}

fn ensure_trailing_newline(mut text: String) -> String {
    if !text.ends_with('\n') {
        text.push('\n');
    }
    text
}
