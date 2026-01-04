//! Parser utilities for the type hash macro.

use std::collections::HashSet;

use cairo_lang_macro::Diagnostic;
use cairo_lang_plugins::plugins::utils::{PluginTypeInfo, TypeVariant};
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_syntax::{node::ast::Attribute, node::db::SyntaxGroup};
use regex::Regex;

use super::definition::TypeHashArgs;
use super::diagnostics::errors;
use super::types::{split_types, InnerType, S12Type};

const SNIP12_TYPE_ATTRIBUTE: &str = "snip12";

/// The parser for the type hash macro.
///
/// It parses the members of the struct or enum and maintains a list of types including referenced objects/enums.
pub struct TypeHashParser<'a> {
    /// The plugin type info object.
    plugin_type_info: &'a PluginTypeInfo,
    /// Quick lookup for already processed types.
    is_type_processed: HashSet<String>,
    /// The encode type of the objects/enums referenced in the input that have already been processed.
    /// HashSet is used to avoid duplicates.
    processed_ref_encoded_types: HashSet<String>,
}

impl<'a> TypeHashParser<'a> {
    /// Creates a new parser for the type hash macro from a plugin type info object.
    pub fn new(plugin_type_info: &'a PluginTypeInfo) -> Self {
        let is_type_processed = HashSet::new();
        let processed_ref_encoded_types = HashSet::new();

        Self {
            plugin_type_info,
            is_type_processed,
            processed_ref_encoded_types,
        }
    }

    /// Parses the object/enum and returns the encoded type.
    pub fn parse(
        &mut self,
        db: &dyn SyntaxGroup,
        args: &TypeHashArgs,
    ) -> Result<String, Diagnostic> {
        // 1. Get the members types real values from mapping and attributes
        let members_types = self
            .plugin_type_info
            .members_info
            .iter()
            .map(|member| {
                let attributes = member.attributes.elements(db);
                let args = match get_name_and_type_from_attributes(db, &attributes) {
                    Ok(args) => args,
                    Err(e) => {
                        return Err(e);
                    }
                };
                let attr_name = args.name;
                let attr_type = args.kind;

                // If there is an attribute, use it, otherwise use the type from the member
                let s12_type = if !attr_type.is_empty() {
                    S12Type::from_str(&attr_type)
                } else {
                    S12Type::from_str(&member.ty)
                };

                // If there is an attribute, use it, otherwise use the name from the member
                let s12_name = if !attr_name.is_empty() {
                    attr_name
                } else {
                    member.name.to_string()
                };

                // Unwrapping should be safe here since attr types must not be empty
                Ok((s12_name, s12_type.unwrap()))
            })
            .collect::<Vec<Result<(String, S12Type), Diagnostic>>>();

        // 2. Build the string representation
        let mut encoded_type = if args.name.is_empty() {
            format!("\"{}\"(", self.plugin_type_info.name)
        } else {
            format!("\"{}\"(", args.name)
        };
        for result in members_types {
            let (name, s12_type) = result?;
            let type_name = s12_type.get_snip12_type_name()?;

            // Format the member depending on the type variant
            match self.plugin_type_info.type_variant {
                TypeVariant::Struct => {
                    encoded_type.push_str(&format!("\"{}\":\"{}\",", name, type_name))
                }
                TypeVariant::Enum => {
                    encoded_type.push_str(&format!("\"{}\"({}),", name, maybe_tuple(&type_name)))
                }
            };

            if !self.is_type_processed.contains(&type_name) {
                let (encoded_type, inner_types) = s12_type.get_encoded_ref_type()?;
                self.processed_ref_encoded_types.insert(encoded_type);
                self.is_type_processed.insert(type_name);

                // Process inner types
                self.process_inner_types(&inner_types);
            }
        }
        if encoded_type.ends_with(",") {
            encoded_type.pop();
        }
        encoded_type.push(')');

        let mut processed_ref_encoded_types =
            self.processed_ref_encoded_types.iter().collect::<Vec<_>>();
        processed_ref_encoded_types.sort();
        for processed_type in processed_ref_encoded_types {
            encoded_type.push_str(processed_type);
        }

        // 3. Return the encoded type
        Ok(encoded_type)
    }

    fn process_inner_types(&mut self, inner_types: &[InnerType]) {
        for inner_type in inner_types {
            if !self.is_type_processed.contains(&inner_type.name) {
                self.processed_ref_encoded_types
                    .insert(inner_type.encoded_type.clone());
                self.is_type_processed.insert(inner_type.name.clone());
            }
        }
    }
}

/// Gets the name and type from the attributes.
///
/// The expected attribute is of the form:
/// ```
/// #[snip12(name: <name>, kind: <type>)]
/// ```
/// or
/// ```
/// #[snip12(kind: <type>)]
/// ```
/// or
/// ```
/// #[snip12(name: <name>)]
fn get_name_and_type_from_attributes(
    db: &dyn SyntaxGroup,
    attributes: &[Attribute],
) -> Result<Snip12Args, Diagnostic> {
    let re = Regex::new(&format!(r"^\#\[{SNIP12_TYPE_ATTRIBUTE}(.*)\]$")).unwrap();

    for attribute in attributes {
        let attribute_text = attribute.as_syntax_node().get_text_without_trivia(db);
        if re.is_match(&attribute_text) {
            let captures = re.captures(&attribute_text);
            if let Some(captures) = captures {
                return parse_snip12_args(&captures[1]);
            }
        }
    }
    Ok(Snip12Args {
        name: "".to_string(),
        kind: "".to_string(),
    })
}

/// Arguments for the snip12 attribute.
///
/// Represents the arguments passed to the snip12 attribute.
///
/// Example:
/// ```
/// #[snip12(name: "MyStruct", kind: "struct")]
/// ```
#[derive(Debug)]
pub struct Snip12Args {
    pub name: String,
    pub kind: String,
}

/// Parses the arguments passed to the snip12 attribute and
/// returns a Snip12Args struct containing the parsed arguments.
fn parse_snip12_args(s: &str) -> Result<Snip12Args, Diagnostic> {
    // Initialize the args with the default values
    let mut args = Snip12Args {
        name: "".to_string(),
        kind: "".to_string(),
    };

    // If the attribute is empty, return the default args
    if s.is_empty() || s == "()" {
        return Ok(args);
    }

    let allowed_args = ["name", "kind"];
    let allowed_args_re = allowed_args.join("|");

    let re = Regex::new(&format!(
        r#"^\(({}): ([\w"'\(\),<> ])+(?:, ({}): ([\w"'\(\),<> ])+)*\)$"#,
        allowed_args_re, allowed_args_re
    ))
    .unwrap();

    if re.is_match(s) {
        // Remove the parentheses
        let s = &s[1..s.len() - 1];

        // Split the string by commas, but not if they are inside parentheses
        // Note that regex doesn't support lookaheads, so we use fancy-regex
        let re = fancy_regex::Regex::new(r",(?![^\(]*\))").unwrap();
        let parts = re.split(s).map(|x| x.unwrap()).collect::<Vec<_>>();

        for arg in parts {
            let parts = arg.split(":").collect::<Vec<&str>>();
            let name = parts[0].trim();
            let value = parts[1].trim();
            match name {
                "name" => args.name = parse_string_arg(value)?,
                "kind" => args.kind = parse_string_arg(value)?,
                // This should be unreachable as long as the regex is correct
                _ => panic!("Invalid argument: {}", name),
            };
        }
        Ok(args)
    } else {
        Err(Diagnostic::error(errors::INVALID_SNIP12_ATTRIBUTE_FORMAT))
    }
}

/// Parses the string argument from the attribute.
pub fn parse_string_arg(s: &str) -> Result<String, Diagnostic> {
    if s.len() >= 3 && s.starts_with("\"") && s.ends_with("\"") {
        // Remove the quotes
        Ok(s[1..s.len() - 1].to_string())
    } else {
        Err(Diagnostic::error(errors::INVALID_STRING_ARGUMENT))
    }
}

/// Returns the enum compliant string representation of a tuple for the encoded type.
///
/// If the input is not a tuple, it returns the input itself.
///
/// Example:
/// ```
/// let encoded_type = maybe_tuple("(felt252, felt252, ClassHash, NftId)");
/// assert_eq!(encoded_type, "\"felt252\",\"felt252\",\"ClassHash\",\"NftId\"");
/// ```
fn maybe_tuple(s: &str) -> String {
    if s.starts_with("(") && s.ends_with(")") {
        split_types(&s[1..s.len() - 1])
            .iter()
            .map(|s| format!("\"{}\"", s.trim()))
            .collect::<Vec<_>>()
            .join(",")
    } else {
        format!("\"{}\"", s)
    }
}
