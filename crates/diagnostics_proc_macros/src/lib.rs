use proc_macro::TokenStream;
use quote::quote;
use syn::token::Mut;
use syn::{AngleBracketedGenericArguments, FnArg, Pat, PatType, Path, TypePath, TypeReference};

/// Macro for defining functions that return WithDiagnostics<T>.
#[proc_macro_attribute]
pub fn with_diagnostics(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse token stream as a function AST.
    let ast: syn::ItemFn = syn::parse(item).unwrap();
    let function_ident = ast.sig.ident;
    let mut params = ast.sig.inputs.into_iter();

    // Extract the first parameter.
    let first_parameter = params.next();

    // Extract Diagnostics type.
    // TODO(spapini): Extract to a function.
    let diagnostics_ref_ty = match first_parameter.unwrap() {
        FnArg::Typed(PatType { pat, ty, .. }) => match &*pat {
            Pat::Ident(ident) => {
                assert_eq!(ident.ident, "diagnostics");
                ty
            }
            _ => panic!("Argument pattern is not a simple ident."),
        },
        FnArg::Receiver(_) => panic!("Argument is a receiver."),
    };
    let diagnostics_ty = match &*diagnostics_ref_ty {
        syn::Type::Reference(TypeReference { mutability: Some(Mut { .. }), elem, .. }) => elem,
        _ => panic!("Expected a reference as a first parameter."),
    };
    let segments = match &**diagnostics_ty {
        syn::Type::Path(TypePath { path: Path { segments, .. }, .. }) => segments,
        _ => panic!("Expected a path as the type for the first arg."),
    };
    let segment = segments.last().unwrap();
    assert_eq!(segment.ident, "Diagnostics");
    let generic_args = match &segment.arguments {
        syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => args,
        _ => panic!("Expected angle brackets, e.g. Diagnostics<T>."),
    };
    let entry_ty = match generic_args.first().unwrap() {
        syn::GenericArgument::Type(ty) => ty,
        _ => panic!("Expected a generic argument for diagnostics."),
    };
    // TODO(spapini): Assert that the first arg is `&mut diagnostics: Diagnostics`.

    // Extract the rest of the arguments into args_syntax.
    let mut args_syntax = quote! {};
    for arg in params {
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
        fn #function_ident(#args_syntax) -> WithDiagnostics<#ret_ty, #entry_ty> {
            let mut diagnostics = Diagnostics::new();
            let f = |diagnostics: &mut Diagnostics<#entry_ty>| {
                #body
            };
            let value = f(&mut diagnostics);
            WithDiagnostics { value, diagnostics }
    }
    }
    .into()
}
