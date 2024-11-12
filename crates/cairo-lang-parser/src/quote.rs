use crate::{
    types::{MockTokenStream, Token, TokenStream, Tokenable},
    utils::SimpleParserDatabase,
};
use cairo_lang_syntax::node::{db::SyntaxGroup, SyntaxNode};
use cairo_lang_utils::Upcast;
use indoc::indoc;

const POSITIONAL_ARG_MARK: &'static str = "{}";

/// Macro that works similar to rust's quote and format macro. It takes a string literal, which includes `{}` sections.
/// Like in format macro in rust, it also takes the same number of arguments as `{}` in a literal. Those values are supposed to be a type of [cairo_lang_syntax::node::SyntaxNode].
/// Macro will produce a new [crate::types::TokenStream], which will persist all the TextSpan values from passed SyntaxNodes inside.
#[macro_export]
macro_rules! quote_format {
  ($db:expr, $literal:expr, $($arg:expr),*) => {{
      let positional_arguments: Vec<_> = $literal.matches(POSITIONAL_ARG_MARK).collect();
      let positional_arguments_number = positional_arguments.len();
      let args_static = [$($arg),*];
      let args = args_static.iter().map(|tokenable| tokenable as &dyn Tokenable).collect::<Vec<_>>();
      let args_num: usize = args.len();

      assert!(
        positional_arguments_number >= args_num,
          "Too many arguments provided for the number of positional arguments. Positional arguments: {}, arguments: {}", positional_arguments_number, args_num
      );
      assert!(
        args_num >= positional_arguments_number,
          "Too many positional arguments for provided arguments. Positional arguments: {}, arguments: {}", positional_arguments_number, args_num
      );

      token_stream_fmt($db, $literal, args)
  }};
}

/// It's based upon an assumption that we have same number of positional args as related args to them.
fn token_stream_fmt(
    db: &dyn SyntaxGroup,
    literal: &str,
    args: Vec<&dyn Tokenable>,
) -> MockTokenStream {
    let mut result_tokens: Vec<Token> = Vec::default();
    let splitted_literal = split_by_space_and_pos_arg(literal);
    let mut offset: u32 = 0;
    let mut arg_index: usize = 0;

    for string_token in splitted_literal.iter() {
        if string_token == POSITIONAL_ARG_MARK {
            let arg = args.get(arg_index).unwrap();
            result_tokens.extend(arg.to_tokens(db));
            offset += arg.len(db);
            arg_index += 1;
        } else {
            result_tokens.push(Token::new_with_offset(string_token, offset));
            offset += string_token.len() as u32;
        }
    }

    MockTokenStream::new(result_tokens)
}

fn split_by_space_and_pos_arg(s: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut last = 0;

    let mut match_indices: Vec<_> =
        s.match_indices(' ').chain(s.match_indices(POSITIONAL_ARG_MARK)).collect();
    match_indices.sort_by_key(|pair| pair.0);

    for (index, matched) in match_indices {
        if last != index {
            result.push(s[last..index].to_owned());
        }
        result.push(matched.to_owned());
        last = index + matched.len();
    }

    if last < s.len() {
        result.push(s[last..].to_owned());
    }
    result
}

// fn format_token_stream(literal: &str, syntax_nodes: Vec<SyntaxNode>) -> TokenStream {}

#[test]
fn test() {
    let db = SimpleParserDatabase::default();
    let root_syntax_node = db
        .parse_virtual(indoc! {"
      fn test_function() {}
    "})
        .unwrap();
  // WAZNE: {} moze tez przyjac samego token'a. Wazne jest to, ze kazdy {} bedize mial pozniej span, ktory bedzie spanem w oryginalnym kodzie. Reszta kodu tak jak nizej np "test" raczej nie bedzie miala spanu albo wgl (sprawdzic w rustcie).
    let a = quote_format!(db.upcast(), "test {}", root_syntax_node);
    println!("result: {:?}", a.tokens);
}
