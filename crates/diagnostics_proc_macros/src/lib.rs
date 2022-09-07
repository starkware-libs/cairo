use proc_macro::TokenStream;
use quote::__private::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::parse::Parse;
use syn::{parse_macro_input, DeriveInput, Ident, Token, TypePath};

/// Macro for defining functions that return WithDiagnostics<T>.
struct WithDiagnosticsMacroInput {
    ty: TypePath,
    name: Ident,
}
impl Parse for WithDiagnosticsMacroInput {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let ty: TypePath = input.parse()?;
        let _comma: Token![,] = input.parse()?;
        let name: Ident = input.parse()?;
        Ok(WithDiagnosticsMacroInput { ty, name })
    }
}
#[proc_macro_attribute]
pub fn with_diagnostics(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse attributes and verify them.
    let WithDiagnosticsMacroInput { ty: path_ty, name: diagnostics_var_name } =
        syn::parse(attr).unwrap();
    assert_eq!(diagnostics_var_name, "diagnostics");
    let TypePath { qself, path } = &path_ty;
    assert!(qself.is_none());
    let segment = path.segments.last().unwrap();
    assert!(segment.arguments.is_empty());

    assert!(
        segment.ident.to_string().ends_with("Diagnostic"),
        "diagnostic entry type must end with 'Diagnostic'"
    );
    let entry_type = &syn::Type::Path(path_ty);

    // Parse `item` TokenStream as a function AST.
    let syn::ItemFn { attrs, vis, sig, block } = syn::parse(item).unwrap();
    let function_ident = sig.ident;
    let params = sig.inputs.into_iter();

    // Extract body and return type.
    let ret_type = match sig.output {
        syn::ReturnType::Default => panic!("No return type."),
        syn::ReturnType::Type(_, ty) => ty,
    };

    // Emit the function.
    quote! {
        #(#attrs)* #vis fn #function_ident(#(#params),*) -> WithDiagnostics<#ret_type, #entry_type> {
            let mut #diagnostics_var_name = Diagnostics::new();

            let mut f = |#diagnostics_var_name: &mut Diagnostics<#entry_type>| -> #ret_type {
                #block
            };
            let value = f(&mut #diagnostics_var_name);
            WithDiagnostics { value, diagnostics: #diagnostics_var_name }
    }
    }
    .into()
}

/// Derives a [diagnostics::debug::DebugWithDb] implementation for structs and enums.
#[proc_macro_derive(DebugWithDb, attributes(debug_db))]
pub fn derive_debug_with_db(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);
    let attribute = input
        .attrs
        .iter()
        .find(|a| a.path.segments.len() == 1 && a.path.segments[0].ident == "debug_db")
        .expect("debug_db attribute required for deriving DebugWithDB.");
    let db: syn::Expr = syn::parse2(attribute.tokens.clone()).expect("Invalid debug_db attribute!");
    let db = if let syn::Expr::Paren(db) = db { db } else { panic!("Expected parenthesis") };
    let db = quote! {(dyn #db + 'static)};
    let name = input.ident;
    match input.data {
        syn::Data::Struct(strct) => emit_struct_debug(name, db, strct),
        syn::Data::Enum(enm) => emit_enum_debug(name, db, enm),
        syn::Data::Union(_) => panic!("Unions are not supported"),
    }
    .into()
}

/// Emits a DebugWithDb implementation for a struct.
fn emit_struct_debug(name: syn::Ident, db: TokenStream2, strct: syn::DataStruct) -> TokenStream2 {
    let (pattern, field_prints) = emit_fields_debug(db.clone(), name.to_string(), strct.fields);
    let crt = debug_crate();
    quote! {
        impl #crt::debug::DebugWithDb<#db> for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &#db) -> std::fmt::Result {
                use #crt::debug::DebugWithDb;
                use #crt::debug::helper::Fallback;
                let #name #pattern = self;
                #field_prints
            }
        }
    }
}

/// Emits a DebugWithDb implementation for an enum.
fn emit_enum_debug(name: syn::Ident, db: TokenStream2, enm: syn::DataEnum) -> TokenStream2 {
    let mut variant_prints = quote! {};
    for variant in enm.variants {
        let variant_name = variant.ident;
        let (pattern, field_prints) =
            emit_fields_debug(db.clone(), variant_name.to_string(), variant.fields);
        variant_prints = quote! {
            #variant_prints
            #name :: #variant_name #pattern => {
                #field_prints
            }
        }
    }
    let crt = debug_crate();
    quote! {
        impl #crt::debug::DebugWithDb<#db> for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &#db) -> std::fmt::Result {
                use #crt::debug::DebugWithDb;
                use #crt::debug::helper::Fallback;
                match self {
                    #variant_prints
                }
            }
        }
    }
}

/// Helper function for emit_struct_debug and emit_enum_debug. Both struct, and a variant use
/// a type called [syn::Fields].
/// This function builds and returns an unpacking pattern and code for `fmt()` on these fields.
fn emit_fields_debug(
    db: TokenStream2,
    name: String,
    fields: syn::Fields,
) -> (TokenStream2, TokenStream2) {
    let crt = debug_crate();
    let mut pattern = quote! {};
    let mut field_prints = quote! {};
    for (i, field) in fields.iter().enumerate() {
        let ty = &field.ty;

        let field_ident = field
            .ident
            .clone()
            .unwrap_or_else(|| syn::Ident::new(&format!("v{i}"), Span::call_site()));
        pattern = quote! { #pattern #field_ident, };
        let func_call = quote! {
                &#crt::debug::helper::HelperDebug::<#ty, #db>::helper_debug(#field_ident, db)
        };
        if let Some(field_ident) = &field.ident {
            field_prints = quote! {
                #field_prints
                .field(stringify!(#field_ident), #func_call)
            }
        } else {
            field_prints = quote! {
                #field_prints
                .field(#func_call)
            }
        }
    }
    match fields {
        syn::Fields::Named(_) => {
            pattern = quote! { { #pattern } };
            field_prints = quote! { f.debug_struct(#name) #field_prints .finish() };
        }
        syn::Fields::Unnamed(_) => {
            pattern = quote! { ( #pattern ) };
            field_prints = quote! { f.debug_tuple(#name) #field_prints .finish() };
        }
        syn::Fields::Unit => {
            pattern = quote! {};
            field_prints = quote! { f.debug_tuple(#name).finish() };
        }
    };
    (pattern, field_prints)
}

/// Returns the identifier of the debug crate. This is needed, since inside the debug
/// crate, it needs to be referred to as `crate` and no `debug`.
fn debug_crate() -> syn::Ident {
    let crate_name = std::env::var("CARGO_PKG_NAME").unwrap();
    let res = if crate_name == "debug" { "crate" } else { "debug" };
    syn::Ident::new(res, Span::call_site())
}
