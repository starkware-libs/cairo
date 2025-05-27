use cairo_lang_utils::debug_macro;
use quote::{ToTokens, format_ident, quote};
use syn::{FnArg, Ident, PatType, Path, Receiver, ReturnType, Type, parse_quote};

pub(crate) struct TrackedQuery {
    pub(crate) trait_name: Ident,
    pub(crate) signature: syn::Signature,
    pub(crate) pat_and_tys: Vec<PatType>,
    pub(crate) invoke: Option<Path>,
    pub(crate) cycle: Option<Path>,
    pub(crate) lru: Option<u32>,
    pub(crate) generated_struct: Option<GeneratedInputStruct>,
}

#[derive(Clone)]
pub(crate) struct GeneratedInputStruct {
    pub(crate) input_struct_name: Ident,
    pub(crate) create_data_ident: Ident,
}

pub(crate) struct CycleFn {
    pub(crate) cycle_fn_name: Ident,
    pub(crate) signature: syn::Signature,
    pub(crate) pat_and_tys: Vec<PatType>,
    pub(crate) trait_name: Ident,
    pub(crate) generated_struct: Option<GeneratedInputStruct>,
}

impl ToTokens for CycleFn {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let cycle_fn_name = &self.cycle_fn_name;
        let sig = &self.signature;
        let trait_name = &self.trait_name;
        let pat_and_tys = &self.pat_and_tys;

        // Extract the return type from the signature
        let return_type = match &sig.output {
            ReturnType::Type(_, ty) => ty.as_ref(),
            ReturnType::Default => {
                // Handle unit type
                let unit_ty: Type = parse_quote!(());
                Box::leak(Box::new(unit_ty))
            }
        };

        // Get the lifetime if any
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
        let lifetimed_trait = if lifetime_tokens.is_empty() {
            quote! {
                dyn #trait_name
            }
        } else {
            quote! {
                (dyn #trait_name + #lifetime_tokens)
            }
        };

        let lifetime_generic = if lifetime_tokens.is_empty() {
            quote! {}
        } else {
            quote! { < #lifetime_tokens > }
        };

        let input_param = {
            if let Some(generated_struct) = &self.generated_struct {
                let input_struct_name = &generated_struct.input_struct_name;
                quote! { _input: #input_struct_name , }
            } else {
                eprintln!(
                    "Tracked with cycle_fn should have a generated struct, but got {:?}",
                    self.cycle_fn_name
                );
                panic!(
                    "Tracked with cycle_fn should have a generated struct, but got {:?}",
                    self.cycle_fn_name
                );
            }
        };

        let function = quote! {
            fn #cycle_fn_name #lifetime_generic(
                db: #lifetimed_ref #lifetimed_trait,
                _cycle: &#return_type,
                _count: u32,
                #input_param
                #(#pat_and_tys),*
            ) -> salsa::CycleRecoveryAction<#return_type> {
                salsa::CycleRecoveryAction::Iterate
            }
        };

        function.to_tokens(tokens);
    }
}

impl TrackedQuery {
    pub(crate) fn cycle_fn(&self) -> Option<CycleFn> {
        if let Some(_cycle_path) = &self.cycle {
            let cycle_fn_name = format_ident!("{}_cycle_fn", self.signature.ident);
            Some(CycleFn {
                cycle_fn_name,
                signature: self.signature.clone(),
                pat_and_tys: self.pat_and_tys.clone(),
                trait_name: self.trait_name.clone(),
                generated_struct: self.generated_struct.clone(),
            })
        } else {
            None
        }
    }
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

        let cycle_fn = format_ident!("{}_cycle_fn", sig.ident);

        let annotation = match (self.cycle.clone(), self.lru) {
            (Some(cycle), Some(lru)) => {
                quote!(#[salsa::tracked(lru = #lru, cycle_fn = #cycle_fn, cycle_initial = #cycle)])
            }
            (Some(cycle), None) => {
                quote!(#[salsa::tracked(cycle_fn = #cycle_fn, cycle_initial = #cycle)])
            }
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

        debug_macro!("Annotations for {} are: \n{}", fn_ident, annotation);

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
        let mut_receiver: Receiver = parse_quote!(&mut self);
        if let Some(first_input) = sig.inputs.first_mut() {
            *first_input = FnArg::Receiver(mut_receiver);
        }

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
        let mut_receiver: Receiver = parse_quote!(&mut self);
        if let Some(first_input) = sig.inputs.first_mut() {
            *first_input = FnArg::Receiver(mut_receiver);
        }

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

        let ty = self.pat_and_tys.to_vec();

        let interned_pat = ty.first().unwrap();
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

        let ty = self.pat_and_tys.to_vec();

        let interned_key = &self.return_ty;

        let interned_pat = ty.first().unwrap();
        let interned_return_ty = &interned_pat.ty;
        let generics = &sig.generics;
        let receiver = sig
            .inputs
            .iter()
            .find(|input| matches!(input, FnArg::Receiver(_)))
            .expect("Expected a receiver on lookup queries");
        self.signature = parse_quote!(
            fn #ident #generics (#receiver, id: #interned_key) -> #interned_return_ty
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
