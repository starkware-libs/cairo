use quote::{format_ident, quote, ToTokens};
use syn::{parse_quote, FnArg, Ident, PatType, Path, Receiver, ReturnType, Type};

pub(crate) struct TrackedQuery {
    pub(crate) trait_name: Ident,
    pub(crate) signature: syn::Signature,
    pub(crate) pat_and_tys: Vec<PatType>,
    pub(crate) invoke: Option<Path>,
    pub(crate) cycle: Option<Path>,
    pub(crate) lru: Option<u32>,
    pub(crate) generated_struct: Option<GeneratedInputStruct>,
}

pub(crate) struct GeneratedInputStruct {
    pub(crate) input_struct_name: Ident,
    pub(crate) create_data_ident: Ident,
}

impl ToTokens for TrackedQuery {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let sig = &self.signature;

        let trait_name = &self.trait_name;

        let ret = &sig.output;

        let invoke = match &self.invoke {
            Some(path) => path.to_token_stream(),
            None => sig.ident.to_token_stream(),
        };

        let fn_ident = &sig.ident;
        let shim: Ident = format_ident!("{}_shim", fn_ident);

        let annotation = match (self.cycle.clone(), self.lru) {
            (Some(cycle), Some(lru)) => quote!(#[salsa::tracked(lru = #lru, recovery_fn = #cycle)]),
            (Some(cycle), None) => quote!(#[salsa::tracked(recovery_fn = #cycle)]),
            (None, Some(lru)) => quote!(#[salsa::tracked(lru = #lru)]),
            (None, None) => quote!(#[salsa::tracked]),
        };

        let pat_and_tys = &self.pat_and_tys;
        let params = self
            .pat_and_tys
            .iter()
            .map(|pat_type| pat_type.pat.clone())
            .collect::<Vec<Box<syn::Pat>>>();

        let lifetime_tokens = if let Some(lifetime) = sig.generics.lifetimes().next() {
            let lifetime = &lifetime.lifetime;
            quote! {
                #lifetime
            }
        } else {
            quote! {}
        };
        let lifetimed_ref = quote! {
            &#lifetime_tokens
        };
        let _lifetimed_trait = if lifetime_tokens.is_empty() {
            quote! {
                dyn #trait_name
            }
        } else {
            quote! {
                (dyn #trait_name + #lifetime_tokens)
            }
        };
        let lifetimed_shim = if lifetime_tokens.is_empty() {
            quote! {
                #shim
            }
        } else {
            quote! {
                #shim<#lifetime_tokens>
            }
        };

        let method = match &self.generated_struct {
            Some(generated_struct) => {
                let input_struct_name = &generated_struct.input_struct_name;
                let create_data_ident = &generated_struct.create_data_ident;

                quote! {
                    #sig {
                        #annotation
                        fn #lifetimed_shim(
                            db: #lifetimed_ref dyn #trait_name,
                            _input: #input_struct_name,
                            #(#pat_and_tys),*
                        ) #ret {
                            #invoke(db, #(#params),*)
                        }
                        #shim(self, #create_data_ident(self), #(#params),*)
                    }
                }
            }
            None => {
                quote! {
                    #sig {
                        #annotation
                        fn #shim(
                            db: &dyn #trait_name,
                            #(#pat_and_tys),*
                        ) #ret {
                            #invoke(db, #(#params),*)
                        }
                        #shim(self, #(#params),*)
                    }
                }
            }
        };

        method.to_tokens(tokens);
    }
}

pub(crate) struct InputQuery {
    pub(crate) signature: syn::Signature,
    pub(crate) create_data_ident: Ident,
}

impl ToTokens for InputQuery {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let sig = &self.signature;
        let fn_ident = &sig.ident;
        let create_data_ident = &self.create_data_ident;

        let method = quote! {
            #sig {
                let data = #create_data_ident(self);
                data.#fn_ident(self).unwrap()
            }
        };
        method.to_tokens(tokens);
    }
}

pub(crate) struct InputSetter {
    pub(crate) signature: syn::Signature,
    pub(crate) return_type: syn::Type,
    pub(crate) create_data_ident: Ident,
}

impl ToTokens for InputSetter {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let sig = &mut self.signature.clone();

        let ty = &self.return_type;
        let fn_ident = &sig.ident;
        let create_data_ident = &self.create_data_ident;

        let setter_ident = format_ident!("set_{}", fn_ident);
        sig.ident = setter_ident.clone();

        let value_argument: PatType = parse_quote!(__value: #ty);
        sig.inputs.push(FnArg::Typed(value_argument.clone()));

        // make `&self` `&mut self` instead.
        let mut_recevier: Receiver = parse_quote!(&mut self);
        sig.inputs
            .first_mut()
            .map(|og| *og = FnArg::Receiver(mut_recevier));

        // remove the return value.
        sig.output = ReturnType::Default;

        let value = &value_argument.pat;
        let method = quote! {
            #sig {
                use salsa::Setter;
                let data = #create_data_ident(self);
                data.#setter_ident(self).to(Some(#value));
            }
        };
        method.to_tokens(tokens);
    }
}

pub(crate) struct InputSetterWithDurability {
    pub(crate) signature: syn::Signature,
    pub(crate) return_type: syn::Type,
    pub(crate) create_data_ident: Ident,
}

impl ToTokens for InputSetterWithDurability {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let sig = &mut self.signature.clone();

        let ty = &self.return_type;
        let fn_ident = &sig.ident;
        let setter_ident = format_ident!("set_{}", fn_ident);

        let create_data_ident = &self.create_data_ident;

        sig.ident = format_ident!("set_{}_with_durability", fn_ident);

        let value_argument: PatType = parse_quote!(__value: #ty);
        sig.inputs.push(FnArg::Typed(value_argument.clone()));

        let durability_argument: PatType = parse_quote!(durability: salsa::Durability);
        sig.inputs.push(FnArg::Typed(durability_argument.clone()));

        // make `&self` `&mut self` instead.
        let mut_recevier: Receiver = parse_quote!(&mut self);
        sig.inputs
            .first_mut()
            .map(|og| *og = FnArg::Receiver(mut_recevier));

        // remove the return value.
        sig.output = ReturnType::Default;

        let value = &value_argument.pat;
        let durability = &durability_argument.pat;
        let method = quote! {
            #sig {
                use salsa::Setter;
                let data = #create_data_ident(self);
                data.#setter_ident(self)
                    .with_durability(#durability)
                    .to(Some(#value));
            }
        };
        method.to_tokens(tokens);
    }
}

pub(crate) enum SetterKind {
    Plain(InputSetter),
    WithDurability(InputSetterWithDurability),
}

impl ToTokens for SetterKind {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            SetterKind::Plain(input_setter) => input_setter.to_tokens(tokens),
            SetterKind::WithDurability(input_setter_with_durability) => {
                input_setter_with_durability.to_tokens(tokens)
            }
        }
    }
}

pub(crate) struct Transparent {
    pub(crate) signature: syn::Signature,
    pub(crate) pat_and_tys: Vec<PatType>,
    pub(crate) invoke: Option<Path>,
}

impl ToTokens for Transparent {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let sig = &self.signature;

        let ty = self
            .pat_and_tys
            .iter()
            .map(|pat_type| pat_type.pat.clone())
            .collect::<Vec<Box<syn::Pat>>>();

        let invoke = match &self.invoke {
            Some(path) => path.to_token_stream(),
            None => sig.ident.to_token_stream(),
        };

        let method = quote! {
            #sig {
                #invoke(self, #(#ty),*)
            }
        };

        method.to_tokens(tokens);
    }
}
pub(crate) struct Intern {
    pub(crate) signature: syn::Signature,
    pub(crate) pat_and_tys: Vec<PatType>,
    pub(crate) interned_struct_path: Path,
}

impl ToTokens for Intern {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let sig = &self.signature;

        let ty = self
            .pat_and_tys
            .iter()
            .map(|pat_type| pat_type.clone())
            .collect::<Vec<syn::PatType>>();

        let interned_pat = ty.iter().next().unwrap();
        let interned_pat = &interned_pat.pat;

        let wrapper_struct = self.interned_struct_path.to_token_stream();

        let method = quote! {
            #sig {
                #wrapper_struct::new(self, #interned_pat)
            }
        };

        method.to_tokens(tokens);
    }
}

pub(crate) struct Lookup {
    pub(crate) signature: syn::Signature,
    pub(crate) pat_and_tys: Vec<PatType>,
    pub(crate) return_ty: Type,
    pub(crate) interned_struct_path: Path,
}

impl Lookup {
    pub(crate) fn prepare_signature(&mut self) {
        let sig = &self.signature;

        let ident = format_ident!("lookup_{}", sig.ident);

        let ty = self
            .pat_and_tys
            .iter()
            .map(|pat_type| pat_type.clone())
            .collect::<Vec<syn::PatType>>();

        let interned_key = &self.return_ty;

        let interned_pat = ty.iter().next().unwrap();
        let interned_return_ty = &interned_pat.ty;
        let generics = &sig.generics;
        let reciever = sig.inputs.iter().find(|input| matches!(input, FnArg::Receiver(_))).expect("Expected a receiver on lookup queries");
        self.signature = parse_quote!(
            fn #ident #generics (#reciever, id: #interned_key) -> #interned_return_ty
        );
    }
}

impl ToTokens for Lookup {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let sig = &self.signature;

        let wrapper_struct = self.interned_struct_path.to_token_stream();
        let method = quote! {
            #sig {
            let id = salsa::plumbing::AsId::as_id(&id);
            #wrapper_struct::ingredient(self).data(self.as_dyn_database(), id).0.clone()
            }
        };

        method.to_tokens(tokens);
    }
}

pub(crate) enum Queries {
    TrackedQuery(TrackedQuery),
    InputQuery(InputQuery),
    Intern(Intern),
    Transparent(Transparent),
}

impl ToTokens for Queries {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Queries::TrackedQuery(tracked_query) => tracked_query.to_tokens(tokens),
            Queries::InputQuery(input_query) => input_query.to_tokens(tokens),
            Queries::Transparent(transparent) => transparent.to_tokens(tokens),
            Queries::Intern(intern) => intern.to_tokens(tokens),
        }
    }
}
