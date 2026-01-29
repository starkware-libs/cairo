use std::fmt;
use std::ops::Range;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use itertools::Itertools;
use pulldown_cmark::{
    Alignment, BrokenLink, CodeBlockKind, Event, HeadingLevel, LinkType, Options,
    Parser as MarkdownParser, Tag, TagEnd,
};

use crate::db::DocGroup;

/// Token representing a link to another item inside the documentation.
#[derive(Debug, PartialEq, Clone, Eq, salsa::Update)]
pub struct CommentLinkToken {
    /// A link part that's inside "[]" brackets.
    pub label: String,
    /// A link part that's inside "()" brackets, right after the label.
    pub path: Option<String>,
    /// The span of the whole link.
    pub link_span: TextSpan,
    /// The span of the destination, if it can be interpreted as a location link.
    pub dest_span: Option<TextSpan>,
    /// Normalized destination text, if it can be interpreted as a location link.
    pub dest_text: Option<String>,
}

/// Generic type for a comment token. It's either plain content or a link.
/// Notice that the Content token type can store much more than just one word.
#[derive(Debug, PartialEq, Clone, Eq, salsa::Update)]
pub enum DocumentationCommentToken {
    /// Token with plain documentation content.
    Content(String),
    /// Link token.
    Link(CommentLinkToken),
}

impl DocumentationCommentToken {
    /// Checks if string representation of [`DocumentationCommentToken`] ends with newline.
    pub fn ends_with_newline(self) -> bool {
        match self {
            DocumentationCommentToken::Content(content) => content.ends_with('\n'),
            DocumentationCommentToken::Link(link_token) => link_token.label.ends_with('\n'),
        }
    }
}

/// Helper struct for formatting possibly nested Markdown lists.
struct DocCommentListItem {
    /// Ordered list item separator
    delimiter: Option<u64>,
    /// Flag for an ordered list
    is_ordered_list: bool,
}

struct PendingLink {
    label: String,
    path: Option<String>,
    link_start: usize,
    link_type: LinkType,
    destination: String,
    label_range: Option<Range<usize>>,
}

/// Parses documentation comment content into a vector of [DocumentationCommentToken]s, keeping
/// the order in which they were present in the content.
///
/// We look for 3 link patterns (ignore the backslash):
/// "\[label\](path)", "\[path\]" or "\[`path`\]".
pub fn parse_documentation_comment(documentation_comment: &str) -> Vec<DocumentationCommentToken> {
    let mut tokens = Vec::new();
    let mut current_link: Option<PendingLink> = None;
    let mut is_indented_code_block = false;
    let mut replacer = |broken_link: BrokenLink<'_>| {
        if matches!(broken_link.link_type, LinkType::ShortcutUnknown | LinkType::Shortcut) {
            return Some((broken_link.reference.to_string().into(), "".into()));
        }
        None
    };

    let mut options = Options::empty();
    options.insert(Options::ENABLE_TABLES);
    let parser = MarkdownParser::new_with_broken_link_callback(
        documentation_comment,
        options,
        Some(&mut replacer),
    );

    let mut list_nesting: Vec<DocCommentListItem> = Vec::new();
    let write_list_item_prefix =
        |list_nesting: &mut Vec<DocCommentListItem>,
         tokens: &mut Vec<DocumentationCommentToken>| {
            if !list_nesting.is_empty() {
                let indent = "  ".repeat(list_nesting.len() - 1);
                let list_nesting = list_nesting.last_mut().unwrap();

                let item_delimiter = if list_nesting.is_ordered_list {
                    let delimiter = list_nesting.delimiter.unwrap_or(0);
                    list_nesting.delimiter = Some(delimiter + 1);
                    format!("{indent}{delimiter}.",)
                } else {
                    format!("{indent}-")
                };
                tokens
                    .push(DocumentationCommentToken::Content(format!("{indent}{item_delimiter} ")));
            }
        };
    let mut prefix_list_item = false;
    let mut last_two_events = [None, None];
    let mut table_alignment: Vec<Alignment> = Vec::new();

    for (event, range) in parser.into_offset_iter() {
        match &event {
            Event::Text(text) => {
                if prefix_list_item {
                    write_list_item_prefix(&mut list_nesting, &mut tokens);
                    prefix_list_item = false;
                }
                if let Some(link) = current_link.as_mut() {
                    link.label.push_str(text.as_ref());
                    link.label_range = Some(range.clone());
                } else {
                    let text = {
                        if is_indented_code_block {
                            format!("    {text}")
                        } else {
                            text.to_string()
                        }
                    };
                    tokens.push(DocumentationCommentToken::Content(text));
                }
            }
            Event::Code(code) => {
                if prefix_list_item {
                    write_list_item_prefix(&mut list_nesting, &mut tokens);
                    prefix_list_item = false;
                }
                let complete_code = format!("`{code}`");
                if let Some(link) = current_link.as_mut() {
                    link.label.push_str(&complete_code);
                    link.label_range = Some(range.clone());
                } else {
                    tokens.push(DocumentationCommentToken::Content(complete_code));
                }
            }
            Event::Start(tag_start) => match tag_start {
                Tag::Heading { level, .. } => {
                    if let Some(last_token) = tokens.last_mut()
                        && !last_token.clone().ends_with_newline()
                    {
                        tokens.push(DocumentationCommentToken::Content("\n".to_string()));
                    }
                    tokens.push(DocumentationCommentToken::Content(format!(
                        "{} ",
                        heading_level_to_markdown(*level)
                    )));
                }
                Tag::List(list_type) => {
                    if !list_nesting.is_empty() {
                        tokens.push(DocumentationCommentToken::Content("\n".to_string()));
                    }
                    list_nesting.push(DocCommentListItem {
                        delimiter: *list_type,
                        is_ordered_list: list_type.is_some(),
                    });
                }
                Tag::CodeBlock(kind) => match kind {
                    CodeBlockKind::Fenced(language) => {
                        if language.trim().is_empty() {
                            tokens.push(DocumentationCommentToken::Content(String::from(
                                "```cairo\n",
                            )));
                        } else {
                            tokens.push(DocumentationCommentToken::Content(format!(
                                "```{language}\n"
                            )));
                        }
                    }
                    CodeBlockKind::Indented => {
                        tokens.push(DocumentationCommentToken::Content("\n".to_string()));
                        is_indented_code_block = true;
                    }
                },
                Tag::Link { link_type, dest_url, .. } => {
                    let path = match *link_type {
                        LinkType::ShortcutUnknown | LinkType::Shortcut => None,
                        _ => Some(dest_url.clone().into_string()),
                    };
                    current_link = Some(PendingLink {
                        label: String::new(),
                        path,
                        link_start: range.start,
                        link_type: *link_type,
                        destination: dest_url.clone().into_string(),
                        label_range: None,
                    });
                }
                Tag::Paragraph | Tag::TableRow => {
                    tokens.push(DocumentationCommentToken::Content("\n".to_string()));
                }
                Tag::Item => {
                    prefix_list_item = true;
                }
                Tag::Table(alignment) => {
                    table_alignment = alignment.clone();
                    tokens.push(DocumentationCommentToken::Content("\n".to_string()));
                }
                Tag::TableCell => {
                    tokens.push(DocumentationCommentToken::Content("|".to_string()));
                }
                Tag::Strong => {
                    tokens.push(DocumentationCommentToken::Content("**".to_string()));
                }
                Tag::Emphasis => {
                    tokens.push(DocumentationCommentToken::Content("_".to_string()));
                }
                _ => {}
            },
            Event::End(tag_end) => match tag_end {
                TagEnd::Heading(_) | TagEnd::Table => {
                    tokens.push(DocumentationCommentToken::Content("\n".to_string()));
                }
                TagEnd::List(_) => {
                    list_nesting.pop();
                }
                TagEnd::Item => {
                    if !matches!(last_two_events[0], Some(Event::End(_)))
                        | !matches!(last_two_events[1], Some(Event::End(_)))
                    {
                        tokens.push(DocumentationCommentToken::Content("\n".to_string()));
                    }
                }
                TagEnd::TableHead => {
                    tokens.push(DocumentationCommentToken::Content(format!(
                        "|\n|{}|",
                        table_alignment
                            .iter()
                            .map(|a| {
                                let (left, right) = get_alignment_markers(a);
                                format!("{left}---{right}")
                            })
                            .join("|")
                    )));
                    table_alignment.clear();
                }
                TagEnd::CodeBlock => {
                    if !is_indented_code_block {
                        tokens.push(DocumentationCommentToken::Content("```\n".to_string()));
                    }
                    is_indented_code_block = false;
                }
                TagEnd::Link => {
                    if let Some(link) = current_link {
                        let link_span = span_from_relative_range(
                            documentation_comment,
                            link.link_start..range.end,
                        );
                        let (dest_span, dest_text) = link
                            .label_range
                            .as_ref()
                            .and_then(|label_range| {
                                location_from_link_fields(
                                    link.link_type,
                                    &link.destination,
                                    label_range,
                                )
                            })
                            .map(|(dest_range, dest_text)| {
                                (
                                    Some(span_from_relative_range(
                                        documentation_comment,
                                        dest_range,
                                    )),
                                    Some(dest_text),
                                )
                            })
                            .unwrap_or((None, None));
                        tokens.push(DocumentationCommentToken::Link(CommentLinkToken {
                            label: link.label,
                            path: link.path,
                            link_span,
                            dest_span,
                            dest_text,
                        }));
                    }
                    current_link = None;
                }
                TagEnd::TableRow => {
                    tokens.push(DocumentationCommentToken::Content("|".to_string()));
                }
                TagEnd::Strong => {
                    tokens.push(DocumentationCommentToken::Content("**".to_string()));
                }
                TagEnd::Emphasis => {
                    tokens.push(DocumentationCommentToken::Content("_".to_string()));
                }
                TagEnd::Paragraph => {
                    tokens.push(DocumentationCommentToken::Content("\n".to_string()));
                }
                _ => {}
            },
            Event::SoftBreak => {
                tokens.push(DocumentationCommentToken::Content("\n".to_string()));
            }
            Event::Rule => {
                tokens.push(DocumentationCommentToken::Content("___\n".to_string()));
            }
            _ => {}
        }
        last_two_events = [last_two_events[1].clone(), Some(event)];
    }

    if let Some(DocumentationCommentToken::Content(token)) = tokens.first()
        && token == "\n"
    {
        tokens.remove(0);
    }
    if let Some(DocumentationCommentToken::Content(token)) = tokens.last_mut() {
        *token = token.trim_end().to_string();
        if token.is_empty() {
            tokens.pop();
        }
    }

    tokens
}

impl fmt::Display for CommentLinkToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.path.clone() {
            Some(path) => write!(f, "[{}]({})", self.label, path),
            None => write!(f, "[{}]", self.label),
        }
    }
}

impl fmt::Display for DocumentationCommentToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DocumentationCommentToken::Content(content) => {
                write!(f, "{content}")
            }
            DocumentationCommentToken::Link(link_token) => {
                write!(f, "{link_token}")
            }
        }
    }
}

impl<'db> DebugWithDb<'db> for CommentLinkToken {
    type Db = dyn DocGroup;
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _db: &Self::Db) -> fmt::Result {
        f.debug_struct("CommentLinkToken")
            .field("label", &self.label)
            .field("path", &self.path)
            .field("dest_text", &self.dest_text)
            .field("dest_span", &self.dest_span)
            .field("link_span", &self.link_span)
            .finish()
    }
}

fn span_from_relative_range(content: &str, range: Range<usize>) -> TextSpan {
    let start = TextOffset::START.add_width(TextWidth::at(content, range.start));
    let end = TextOffset::START.add_width(TextWidth::at(content, range.end));
    TextSpan::new(start, end)
}

fn location_from_link_fields(
    link_type: LinkType,
    destination: &str,
    label_range: &Range<usize>,
) -> Option<(Range<usize>, String)> {
    let (destination_normalized, backticked) = normalize_location_text(destination)?;

    match link_type {
        LinkType::Inline => {
            let range = find_inline_destination_range(label_range.end, destination);
            Some((range, destination_normalized))
        }
        LinkType::Collapsed
        | LinkType::CollapsedUnknown
        | LinkType::Shortcut
        | LinkType::ShortcutUnknown => Some((label_range.clone(), destination_normalized)),
        _ => None,
    }
    .map(|(range, text)| (trim_backtick_range(range.clone(), backticked), text))
}

fn is_location_string(value: &str) -> bool {
    !value.is_empty() && value.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == ':')
}

fn normalize_location_text(value: &str) -> Option<(String, bool)> {
    let (value, backticked) = strip_backticks(value);
    is_location_string(value).then(|| (value.to_string(), backticked))
}

fn strip_backticks(value: &str) -> (&str, bool) {
    let value = value.trim();
    if let Some(stripped) = value.strip_prefix('`').and_then(|rest| rest.strip_suffix('`')) {
        (stripped, true)
    } else {
        (value, false)
    }
}

fn trim_backtick_range(range: Range<usize>, backticked: bool) -> Range<usize> {
    if backticked { (range.start + 1)..(range.end - 1) } else { range }
}

fn find_inline_destination_range(label_last_end: usize, destination: &str) -> Range<usize> {
    let destination_start = label_last_end + 2;
    let destination_end = destination_start + destination.len();
    destination_start..destination_end
}

/// Maps `HeadingLevel` to the correct markdown marker.
fn heading_level_to_markdown(heading_level: HeadingLevel) -> String {
    let heading_char: String = String::from("#");
    match heading_level {
        HeadingLevel::H1 => heading_char,
        HeadingLevel::H2 => heading_char.repeat(2),
        HeadingLevel::H3 => heading_char.repeat(3),
        HeadingLevel::H4 => heading_char.repeat(4),
        HeadingLevel::H5 => heading_char.repeat(5),
        HeadingLevel::H6 => heading_char.repeat(6),
    }
}

/// Maps [`Alignment`] to the correct markdown markers.
fn get_alignment_markers(alignment: &Alignment) -> (String, String) {
    let (left, right) = match alignment {
        Alignment::None => ("", ""),
        Alignment::Left => (":", ""),
        Alignment::Right => ("", ":"),
        Alignment::Center => (":", ":"),
    };
    (left.to_string(), right.to_string())
}
