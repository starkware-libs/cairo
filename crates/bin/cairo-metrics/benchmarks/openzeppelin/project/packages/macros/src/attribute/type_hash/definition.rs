use cairo_lang_formatter::format_string;
use cairo_lang_macro::{attribute_macro, Diagnostic, Diagnostics, ProcMacroResult, TokenStream};
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_plugins::plugins::utils::PluginTypeInfo;
use cairo_lang_starknet_classes::keccak::starknet_keccak;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_syntax::node::{db::SyntaxGroup, SyntaxNode};
use convert_case::{Case, Casing};
use indoc::formatdoc;
use regex::Regex;

use crate::type_hash::parser::TypeHashParser;

use super::diagnostics::errors;
use super::parser::parse_string_arg;

/// Derive macro that generates a SNIP-12 type hash constant for a struct.
///
/// Example:
/// ```
/// #[type_hash]
/// pub struct MyStruct {
///     pub some_member: felt252,
/// }
///
/// // Generates:
/// pub const MY_STRUCT_TYPE_HASH: felt252 = 0x[HASH];
/// ```
#[attribute_macro]
pub fn type_hash(attr_stream: TokenStream, item_stream: TokenStream) -> ProcMacroResult {
    // 1. Parse the attribute stream
    let config = match parse_args(&attr_stream.to_string()) {
        Ok(config) => config,
        Err(err) => {
            return ProcMacroResult::new(TokenStream::empty()).with_diagnostics(err.into());
        }
    };

    // 2. Parse the item stream
    let db = SimpleParserDatabase::default();
    let content = match db.parse_virtual(item_stream.to_string()) {
        Ok(node) => handle_node(&db, node, &config),
        Err(err) => {
            let error = Diagnostic::error(err.format(&db));
            return ProcMacroResult::new(TokenStream::empty()).with_diagnostics(error.into());
        }
    };

    // 3. Format the expanded content
    let (formatted_content, diagnostics) = match content {
        Ok(content) => (format_string(&db, content), Diagnostics::new(vec![])),
        Err(err) => (String::new(), err.into()),
    };

    // 4. Return the result
    let result = TokenStream::new(item_stream.to_string() + &formatted_content);
    ProcMacroResult::new(result).with_diagnostics(diagnostics)
}

/// This attribute macro is used to specify an override for the SNIP-12 type.
///
/// It doesn't modify the source code directly, but it is used in the type hash parser to generate the new type hash.
///
/// Example:
/// ```
/// #[type_hash]
/// pub struct MyStruct {
///     #[snip12(name: "Some Member", kind: "shortstring")]
///     pub some_member: felt252,
/// }
/// ```
#[attribute_macro]
pub fn snip12(attr_stream: TokenStream, item_stream: TokenStream) -> ProcMacroResult {
    // Validate the received format
    let re1 = Regex::new(r"^\(name: (.*), kind: (.*)\)$").unwrap();
    let re2 = Regex::new(r"^\(kind: (.*), name: (.*)\)$").unwrap();
    let re3 = Regex::new(r"^\(kind: (.*)\)$").unwrap();
    let re4 = Regex::new(r"^\(name: (.*)\)$").unwrap();
    let s = attr_stream.to_string();

    if re1.is_match(&s) || re2.is_match(&s) || re3.is_match(&s) || re4.is_match(&s) {
        ProcMacroResult::new(item_stream)
    } else {
        let error = Diagnostic::error(errors::INVALID_SNIP12_ATTRIBUTE_FORMAT);
        ProcMacroResult::new(item_stream).with_diagnostics(error.into())
    }
}

/// Configuration for the type hash attribute.
///
/// Represents the arguments passed to the type_hash attribute.
///
/// Example:
/// ```
/// #[type_hash(name: "MyStruct", debug: true)]
/// ```
pub struct TypeHashArgs {
    pub name: String,
    pub debug: bool,
}

/// Parses the arguments passed to the type_hash attribute and
/// returns a TypeHashArgs struct containing the parsed arguments.
fn parse_args(s: &str) -> Result<TypeHashArgs, Diagnostic> {
    let mut args = TypeHashArgs {
        name: String::new(),
        debug: false,
    };
    // If the attribute is empty, return the default config
    if s.is_empty() || s == "()" {
        return Ok(args);
    }

    let allowed_args = ["name", "debug"];
    let allowed_args_re = allowed_args.join("|");

    let re = Regex::new(&format!(
        r#"^\(({}): ([\w"' ])+(?:, ({}): ([\w"' ])+)*\)$"#,
        allowed_args_re, allowed_args_re
    ))
    .unwrap();

    if re.is_match(s) {
        // Remove the parentheses
        let s = &s[1..s.len() - 1];
        for arg in s.split(",") {
            let parts = arg.split(":").collect::<Vec<&str>>();
            let name = parts[0].trim();
            let value = parts[1].trim();
            match name {
                "name" => args.name = parse_string_arg(value)?,
                "debug" => args.debug = value == "true",
                // This should be unreachable as long as the regex is correct
                _ => panic!("Invalid argument: {}", name),
            }
        }
        Ok(args)
    } else {
        Err(Diagnostic::error(
            errors::INVALID_TYPE_HASH_ATTRIBUTE_FORMAT,
        ))
    }
}

fn handle_node(
    db: &dyn SyntaxGroup,
    node: SyntaxNode,
    args: &TypeHashArgs,
) -> Result<String, Diagnostic> {
    let typed = ast::SyntaxFile::from_syntax_node(db, node);
    let items = typed.items(db).elements(db);

    let Some(item_ast) = items.first() else {
        let error = Diagnostic::error(errors::EMPTY_TYPE_FOUND);
        return Err(error);
    };

    // Generate type hash for structs/enums only
    match item_ast {
        ast::ModuleItem::Struct(_) | ast::ModuleItem::Enum(_) => {
            // It is safe to unwrap here because we know the item is a struct
            let plugin_type_info = PluginTypeInfo::new(db, item_ast).unwrap();
            generate_code(db, &plugin_type_info, args)
        }
        _ => {
            let error = Diagnostic::error(errors::NOT_VALID_TYPE_TO_DECORATE);
            Err(error)
        }
    }
}

/// Generates the code for the type hash constant.
fn generate_code(
    db: &dyn SyntaxGroup,
    plugin_type_info: &PluginTypeInfo,
    args: &TypeHashArgs,
) -> Result<String, Diagnostic> {
    let mut parser = TypeHashParser::new(plugin_type_info);
    let type_hash_string = parser.parse(db, args)?;
    let type_hash = starknet_keccak(type_hash_string.as_bytes());
    let type_name = plugin_type_info.name.as_str().to_case(Case::UpperSnake);

    let debug_string = if args.debug {
        formatdoc!(
            r#"
            pub fn __{type_name}_encoded_type() {{
                println!("{}");
            }}"#,
            type_hash_string.replace("\"", "\\\"")
        )
    } else {
        String::new()
    };

    let code = formatdoc!(
        "
        {debug_string}
        pub const {type_name}_TYPE_HASH: felt252 = 0x{:x};

        ",
        type_hash
    );

    Ok(code)
}
