use proc_macro::TokenStream;
use quote::__private::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Parser};
use syn::punctuated::Punctuated;
use syn::{parenthesized, token, Error, Expr, LitStr, Token};

// A macro to replace #[test], which allows logging.
#[proc_macro_attribute]
pub fn test(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_fn = syn::parse_macro_input!(item as syn::ItemFn);
    proc_macro::TokenStream::from(quote! {
        #[test_log::test]
        #item_fn
    })
}

/// Represents a single test-case of the form: `(args...; "name")`.
struct WrappedTestCase {
    _paren_token: token::Paren,
    test_case: Punctuated<Expr, Token![,]>,
    _semi: Token![;],
    name: LitStr,
}
impl Parse for WrappedTestCase {
    fn parse(input: ParseStream<'_>) -> Result<Self, Error> {
        let content;
        Ok(WrappedTestCase {
            _paren_token: parenthesized!(content in input),
            test_case: Punctuated::parse_separated_nonempty_with(&content, Expr::parse)?,
            _semi: content.parse()?,
            name: content.parse()?,
        })
    }
}
impl ToTokens for WrappedTestCase {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let test_case = &self.test_case;
        let name = &self.name;
        let all = quote! { (#test_case; #name) };
        tokens.extend(all);
    }
}

/// A comma-separated array of `WrappedTestCase`s. e.g.: `(1, "case1"), (2, "case2")`.
struct TestCaseArray {
    pub test_cases: Punctuated<WrappedTestCase, Token![,]>,
}
impl Parse for TestCaseArray {
    fn parse(input: ParseStream<'_>) -> Result<Self, Error> {
        Ok(TestCaseArray {
            test_cases: Punctuated::parse_separated_nonempty_with(input, WrappedTestCase::parse)?,
        })
    }
}

/// A macro to replace #[test_case], which allows logging.
/// The syntax is similar but not the same: instead of having multiple `#[test_case(...)]`
/// attributes, use a single `#[test_case_log((1, "case1"), (2, "case2"))]` attribute.
#[proc_macro_attribute]
pub fn test_case_log(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_fn = syn::parse_macro_input!(item as syn::ItemFn);

    let mut all_test_cases = quote! {};
    let parser = TestCaseArray::parse;
    let test_case_arr = parser.parse(attr).expect("unexpected macro failure");

    for wrapped_test_case in test_case_arr.test_cases {
        let test_case = wrapped_test_case.test_case;
        all_test_cases = quote! {
            #all_test_cases
            #[test_case(#test_case)]
        };
    }

    proc_macro::TokenStream::from(quote! {
        #all_test_cases
        #[test_log::test]
        #item_fn
    })
}
