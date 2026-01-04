use cairo_lang_formatter::format_string;
use cairo_lang_macro::{attribute_macro, Diagnostic, ProcMacroResult, TokenStream};
use cairo_lang_parser::utils::SimpleParserDatabase;
use regex::Regex;

use super::{components::AllowedComponents, parser::WithComponentsParser};

/// Inserts multiple component dependencies into a modules codebase.
#[attribute_macro]
pub fn with_components(attribute_stream: TokenStream, item_stream: TokenStream) -> ProcMacroResult {
    let args = parse_args(&attribute_stream.to_string());

    // 1. Get the components info (if valid)
    let mut components_info = vec![];
    let empty_result = ProcMacroResult::new(TokenStream::empty());
    for arg in args {
        let maybe_component = AllowedComponents::from_str(&arg);
        match maybe_component {
            Ok(component) => {
                components_info.push(component.get_info());
            }
            Err(err) => {
                return empty_result.with_diagnostics(err.into());
            }
        }
    }

    // 2. Parse the item stream
    let db = SimpleParserDatabase::default();
    let (content, diagnostics) = match db.parse_virtual(item_stream.to_string()) {
        Ok(node) => WithComponentsParser::new(node, &components_info).parse(&db),
        Err(err) => {
            let error = Diagnostic::error(err.format(&db));
            return empty_result.with_diagnostics(error.into());
        }
    };

    let formatted_content = if !content.is_empty() {
        format_string(&db, content)
    } else {
        content
    };

    ProcMacroResult::new(TokenStream::new(formatted_content)).with_diagnostics(diagnostics)
}

/// Parses the arguments from the attribute stream.
fn parse_args(text: &str) -> Vec<String> {
    let re = Regex::new(r"(\w+)").unwrap();
    let matches = re.find_iter(text);
    matches.map(|m| m.as_str().to_string()).collect()
}

#[cfg(test)]
mod tests {
    use super::parse_args;

    #[test]
    fn test_parse_args() {
        let attribute = "(ERC20, Ownable)";
        let result = parse_args(attribute);
        assert_eq!(result, vec!["ERC20", "Ownable"]);

        let attribute = "ERC20";
        let result = parse_args(attribute);
        assert_eq!(result, vec!["ERC20"]);

        let attribute = "(Ownable, ERC20, Other, Another)";
        let result = parse_args(attribute);
        assert_eq!(result, vec!["Ownable", "ERC20", "Other", "Another"]);
    }
}
