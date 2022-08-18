use proc_macro::TokenStream;
use quote::quote;
use syn::{FnArg, Pat, PatType};

/// Macro for defining functions that return WithDiagnostics<T>.
#[proc_macro_attribute]
pub fn with_diagnostics(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse token stream as a function AST.
    let ast: syn::ItemFn = syn::parse(item).unwrap();
    let function_ident = ast.sig.ident;
    let mut args = ast.sig.inputs.into_iter();

    // Extract the first argument.
    let _first_argument = args.next();
    // TODO(spapini): Assert that the first arg is `&mut diagnostics: Diagnostics`.

    // Extract the rest of the arguments into args_syntax.
    let mut args_syntax = quote! {};
    for arg in args {
        match arg {
            FnArg::Typed(PatType { pat, ty, .. }) => match &*pat {
                Pat::Ident(ident) => args_syntax = quote! {#args_syntax #ident: #ty,},
                _ => panic!("Argument pattern is not a simple ident."),
            },
            FnArg::Receiver(_) => panic!("Argument is a receiver."),
        }
    }

    // Extract body and return type.
    let body = ast.block;
    let ret_ty = match ast.sig.output {
        syn::ReturnType::Default => panic!("No return type."),
        syn::ReturnType::Type(_, ty) => ty,
    };

    // Emit a wrapper function.
    quote! {
        fn #function_ident(#args_syntax) -> WithDiagnostics<#ret_ty> {
            let mut diagnostics = Diagnostics::default();
            let f = |diagnostics: &mut Diagnostics| {
                #body
            };
            let value = f(&mut diagnostics);
            WithDiagnostics { value, diagnostics }
    }
    }
    .into()
}
