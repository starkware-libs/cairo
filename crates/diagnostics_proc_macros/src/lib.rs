use proc_macro::TokenStream;
use quote::quote;
use syn::{FnArg, Pat, PatType};

#[proc_macro_attribute]
pub fn with_diagnostics(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast: syn::ItemFn = syn::parse(item).unwrap();
    let function_ident = ast.sig.ident;
    let mut args = ast.sig.inputs.into_iter();
    args.next();
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
    let body = ast.block;
    let ret_ty = match ast.sig.output {
        syn::ReturnType::Default => panic!("No return type."),
        syn::ReturnType::Type(_, ty) => ty,
    };
    quote! {
        fn #function_ident(#args_syntax) -> WithDiagnostics<#ret_ty> {
            let mut bag = DiagnosticBag::default();
            let f = |bag: &mut DiagnosticBag| {
                #body
            };
            let value = f(&mut bag);
            WithDiagnostics { value, bag }
    }
    }
    .into()
}
