use std::collections::{HashMap, HashSet};

use cairo_lang_defs::ids::{
    FunctionWithBodyId, ImplAliasId, ImplDefId, LanguageElementId, ModuleId, ModuleItemId,
    NamedLanguageElementId, SubmoduleId, TopLevelLanguageElementId, TraitFunctionId, TraitId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_semantic::corelib::core_submodule;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::attribute::SemanticQueryAttrs;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::imp::{ImplLongId, ImplLookupContext};
use cairo_lang_semantic::types::{ConcreteEnumLongId, ConcreteStructLongId, get_impl_at_context};
use cairo_lang_semantic::{
    ConcreteTraitLongId, ConcreteTypeId, GenericArgumentId, GenericParam, Mutability, Signature,
    TypeId, TypeLongId,
};
use cairo_lang_starknet_classes::abi::{
    Constructor, Contract, Enum, EnumVariant, Event, EventField, EventFieldKind, EventKind,
    Function, Imp, Input, Interface, Item, L1Handler, Output, StateMutability, Struct,
    StructMember,
};
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::{Intern, LookupIntern, require, try_extract_matches};
use itertools::zip_eq;
use smol_str::SmolStr;
use thiserror::Error;

use crate::plugin::aux_data::StarknetEventAuxData;
use crate::plugin::consts::{
    ABI_ATTR, ABI_ATTR_EMBED_V0_ARG, ABI_ATTR_PER_ITEM_ARG, ACCOUNT_CONTRACT_ENTRY_POINT_SELECTORS,
    CONSTRUCTOR_ATTR, CONTRACT_ATTR, CONTRACT_ATTR_ACCOUNT_ARG, CONTRACT_STATE_NAME,
    EMBEDDABLE_ATTR, EVENT_ATTR, EVENT_TYPE_NAME, EXTERNAL_ATTR, FLAT_ATTR, INTERFACE_ATTR,
    L1_HANDLER_ATTR, VALIDATE_DEPLOY_ENTRY_POINT_SELECTOR,
};
use crate::plugin::events::EventData;

#[cfg(test)]
#[path = "abi_test.rs"]
mod test;

/// Event information.
enum EventInfo {
    /// The event is a struct.
    Struct,
    /// The event is an enum, contains its set of selectors.
    Enum(HashSet<String>),
}

/// The information of an entrypoint.
struct EntryPointInfo {
    /// The source of the entry point.
    source: Source,
    /// The signature of the entry point.
    inputs: Vec<Input>,
}

#[derive(Clone, Debug, Default)]
/// Configuration for the abi builder.
pub struct BuilderConfig {
    /// Whether to run account contract validations.
    pub account_contract_validations: bool,
}

pub struct AbiBuilder<'a> {
    /// The db.
    db: &'a dyn SemanticGroup,
    /// The builder configuration.
    config: BuilderConfig,

    // TODO(spapini): Add storage variables.
    /// The constructed ABI.
    abi_items: OrderedHashSet<Item>,

    /// List of type that were included abi.
    /// Used to avoid redundancy.
    types: HashSet<TypeId>,

    /// A map of events that were included in the abi to their info.
    /// Used to avoid redundancy, as well as preventing enum events from repeating selectors.
    event_info: HashMap<TypeId, EventInfo>,

    /// List of entry point names that were included in the abi.
    /// Used to avoid duplication.
    entry_points: HashMap<String, EntryPointInfo>,

    /// The constructor for the contract.
    ctor: Option<EntryPointInfo>,

    /// Accumulated errors.
    errors: Vec<ABIError>,
}
impl<'a> AbiBuilder<'a> {
    /// Creates an `AbiBuilder` from a Starknet contract module.
    pub fn from_submodule(
        db: &'a dyn SemanticGroup,
        submodule_id: SubmoduleId,
        config: BuilderConfig,
    ) -> Maybe<Self> {
        let mut builder = Self {
            db,
            config,
            abi_items: Default::default(),
            types: HashSet::new(),
            event_info: HashMap::new(),
            entry_points: HashMap::new(),
            ctor: None,
            errors: Vec::new(),
        };
        builder.process_submodule_contract(submodule_id)?;
        builder.account_contract_validations(submodule_id)?;
        Ok(builder)
    }

    /// Returns the finalized ABI.
    pub fn finalize(self) -> Result<Contract, ABIError> {
        if let Some(err) = self.errors.into_iter().next() {
            Err(err)
        } else {
            Ok(Contract::from_items(self.abi_items))
        }
    }

    /// Returns the errors accumulated by the builder.
    pub fn errors(&self) -> &[ABIError] {
        &self.errors
    }

    /// Runs account contract validations if required.
    fn account_contract_validations(&mut self, submodule_id: SubmoduleId) -> Maybe<()> {
        if !self.config.account_contract_validations {
            return Ok(());
        }
        let attrs = submodule_id.query_attr(self.db, CONTRACT_ATTR)?;
        let mut is_account_contract = false;
        for attr in attrs {
            if attr.is_single_unnamed_arg(self.db.upcast(), CONTRACT_ATTR_ACCOUNT_ARG) {
                is_account_contract = true;
            } else if !attr.args.is_empty() {
                self.errors.push(ABIError::IllegalContractAttrArgs);
                return Ok(());
            }
        }
        if is_account_contract {
            for selector in ACCOUNT_CONTRACT_ENTRY_POINT_SELECTORS {
                if !self.entry_points.contains_key(*selector) {
                    self.errors.push(ABIError::EntryPointMissingForAccountContract {
                        selector: selector.to_string(),
                    });
                }
            }
            if let Some(validate_deploy) =
                self.entry_points.get(VALIDATE_DEPLOY_ENTRY_POINT_SELECTOR)
            {
                let ctor_inputs =
                    self.ctor.as_ref().map(|ctor| ctor.inputs.as_slice()).unwrap_or(&[]);
                if !validate_deploy.inputs.ends_with(ctor_inputs) {
                    self.errors.push(ABIError::ValidateDeployMismatchingConstructor(
                        validate_deploy.source,
                    ));
                }
            }
        } else {
            for selector in ACCOUNT_CONTRACT_ENTRY_POINT_SELECTORS {
                if let Some(info) = self.entry_points.get(*selector) {
                    self.errors.push(ABIError::EntryPointSupportedOnlyOnAccountContract {
                        selector: selector.to_string(),
                        source_ptr: info.source,
                    });
                }
            }
        }
        Ok(())
    }

    /// Adds a Starknet contract module to the ABI.
    fn process_submodule_contract(&mut self, submodule_id: SubmoduleId) -> Maybe<()> {
        let mut free_functions = Vec::new();
        let mut enums = Vec::new();
        let mut structs = Vec::new();
        let mut impl_defs = Vec::new();
        let mut impl_aliases = Vec::new();
        for item in &*self.db.module_items(ModuleId::Submodule(submodule_id))? {
            match item {
                ModuleItemId::FreeFunction(id) => free_functions.push(*id),
                ModuleItemId::Struct(id) => structs.push(*id),
                ModuleItemId::Enum(id) => enums.push(*id),
                ModuleItemId::Impl(id) => impl_defs.push(*id),
                ModuleItemId::ImplAlias(id) => impl_aliases.push(*id),
                _ => {}
            }
        }

        // Get storage type for later validations.
        let mut storage_type = None;
        for struct_id in structs {
            let struct_name = struct_id.name(self.db.upcast());
            let concrete_struct_id =
                ConcreteStructLongId { struct_id, generic_args: vec![] }.intern(self.db);
            let source = Source::Struct(concrete_struct_id);
            if struct_name == CONTRACT_STATE_NAME {
                if storage_type.is_some() {
                    self.errors.push(ABIError::MultipleStorages(source));
                }
                storage_type = Some(
                    TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id))
                        .intern(self.db),
                );
            }
            // Forbid a struct named Event.
            if struct_name == EVENT_TYPE_NAME {
                self.errors.push(ABIError::EventMustBeEnum(source));
            }
        }
        let Some(storage_type) = storage_type else {
            self.errors.push(ABIError::NoStorage);
            return Ok(());
        };

        // Add impls to ABI.
        for impl_def in impl_defs {
            let source = Source::Impl(impl_def);
            let is_of_interface =
                self.db.impl_def_trait(impl_def)?.has_attr(self.db, INTERFACE_ATTR)?;
            // TODO(v3): deprecate the external attribute.
            if impl_def.has_attr(self.db, EXTERNAL_ATTR)? {
                if is_of_interface {
                    self.add_embedded_impl(source, impl_def, None)
                        .unwrap_or_else(|err| self.errors.push(err));
                } else {
                    self.add_non_interface_impl(source, impl_def, storage_type)
                        .unwrap_or_else(|err| self.errors.push(err));
                }
            } else if is_impl_abi_embed(self.db, impl_def)? {
                if !is_of_interface {
                    self.errors.push(ABIError::EmbeddedImplMustBeInterface(source));
                }
                self.add_embedded_impl(source, impl_def, None)
                    .unwrap_or_else(|err| self.errors.push(err));
            } else if is_impl_abi_per_item(self.db, impl_def)? {
                if is_of_interface {
                    self.errors.push(ABIError::ContractInterfaceImplCannotBePerItem(source));
                }
                self.add_per_item_impl(impl_def, storage_type)
                    .unwrap_or_else(|err| self.errors.push(err));
            }
        }
        for impl_alias in impl_aliases {
            if impl_alias.has_attr_with_arg(self.db, ABI_ATTR, ABI_ATTR_EMBED_V0_ARG)? {
                self.add_embedded_impl_alias(impl_alias)
                    .unwrap_or_else(|err| self.errors.push(err));
            }
        }

        // Add external functions, constructor and L1 handlers to ABI.
        for free_function_id in free_functions {
            self.maybe_add_function_with_body(
                FunctionWithBodyId::Free(free_function_id),
                storage_type,
            )
            .unwrap_or_else(|err| self.errors.push(err));
        }

        // Add events to ABI.
        for enum_id in enums {
            let enm_name = enum_id.name(self.db.upcast());
            if enm_name == EVENT_TYPE_NAME && enum_id.has_attr(self.db.upcast(), EVENT_ATTR)? {
                // Get the ConcreteEnumId from the EnumId.
                let concrete_enum_id =
                    ConcreteEnumLongId { enum_id, generic_args: vec![] }.intern(self.db);
                let source = Source::Enum(concrete_enum_id);
                // Check that the enum has no generic parameters.
                if !self.db.enum_generic_params(enum_id).unwrap_or_default().is_empty() {
                    self.errors.push(ABIError::EventWithGenericParams(source));
                }
                // Get the TypeId of the enum.
                let ty =
                    TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)).intern(self.db);
                self.add_event(ty, source).unwrap_or_else(|err| self.errors.push(err));
            }
        }
        Ok(())
    }

    /// Adds an interface to the ABI.
    fn add_interface(&mut self, source: Source, trait_id: TraitId) -> Result<(), ABIError> {
        // Get storage type
        let generic_params = self.db.trait_generic_params(trait_id)?;
        let [GenericParam::Type(storage_type)] = generic_params.as_slice() else {
            return Err(ABIError::ExpectedOneGenericParam(source));
        };
        let storage_type = TypeLongId::GenericParameter(storage_type.id).intern(self.db);

        let interface_path = trait_id.full_path(self.db.upcast());
        let mut items = Vec::new();
        for function in self.db.trait_functions(trait_id).unwrap_or_default().values() {
            let f = self.trait_function_as_abi(*function, storage_type)?;
            self.add_entry_point(
                function.name(self.db.upcast()).into(),
                EntryPointInfo { source, inputs: f.inputs.clone() },
            )?;
            items.push(Item::Function(f));
        }

        let interface_item = Item::Interface(Interface { name: interface_path.clone(), items });
        self.add_abi_item(interface_item, true, source)?;

        Ok(())
    }

    /// Adds the functions of an `external` impl of non-interface trait to the ABI as external
    /// functions.
    fn add_non_interface_impl(
        &mut self,
        source: Source,
        impl_def_id: ImplDefId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        let trait_id = self.db.impl_def_trait(impl_def_id)?;
        for function in self.db.trait_functions(trait_id).unwrap_or_default().values() {
            let function_abi = self.trait_function_as_abi(*function, storage_type)?;
            self.add_abi_item(Item::Function(function_abi), true, source)?;
        }

        Ok(())
    }

    /// Adds an impl of a starknet interface to the ABI.
    /// `impl_alias_name` can override the given impl name and is used in the ABI if set.
    fn add_embedded_impl(
        &mut self,
        source: Source,
        impl_def_id: ImplDefId,
        impl_alias_name: Option<String>,
    ) -> Result<(), ABIError> {
        let impl_name = impl_def_id.name(self.db.upcast());

        let trt = self.db.impl_def_concrete_trait(impl_def_id)?;

        let trait_id = trt.trait_id(self.db);
        let interface_name = trait_id.full_path(self.db.upcast());

        let abi_name = impl_alias_name.unwrap_or(impl_name.into());
        let impl_item = Item::Impl(Imp { name: abi_name, interface_name });
        self.add_abi_item(impl_item, true, source)?;
        self.add_interface(source, trait_id)?;

        Ok(())
    }

    /// Adds an embedded impl to the ABI.
    fn add_per_item_impl(
        &mut self,
        impl_def_id: ImplDefId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        for impl_function_id in self.db.impl_functions(impl_def_id).unwrap_or_default().values() {
            self.maybe_add_function_with_body(
                FunctionWithBodyId::Impl(*impl_function_id),
                storage_type,
            )?;
        }
        Ok(())
    }

    /// Adds an embedded impl alias to the ABI.
    fn add_embedded_impl_alias(&mut self, impl_alias_id: ImplAliasId) -> Result<(), ABIError> {
        let source = Source::ImplAlias(impl_alias_id);
        let impl_def = self.db.impl_alias_impl_def(impl_alias_id)?;

        // Verify the impl definition has #[starknet::embeddable].
        if !impl_def.has_attr(self.db, EMBEDDABLE_ATTR)? {
            return Err(ABIError::EmbeddedImplNotEmbeddable(source));
        }

        // Verify the trait is marked as #[starknet::interface].
        if !self.db.impl_def_trait(impl_def)?.has_attr(self.db, INTERFACE_ATTR)? {
            return Err(ABIError::EmbeddedImplMustBeInterface(source));
        }

        // Add the impl to the ABI.
        self.add_embedded_impl(
            source,
            impl_def,
            Some(impl_alias_id.name(self.db.upcast()).into()),
        )?;

        Ok(())
    }

    /// Adds a function to the ABI according to its attributes.
    fn maybe_add_function_with_body(
        &mut self,
        function_with_body_id: FunctionWithBodyId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        if function_with_body_id.has_attr(self.db.upcast(), EXTERNAL_ATTR)? {
            self.add_function_with_body(function_with_body_id, storage_type)?;
        } else if function_with_body_id.has_attr(self.db.upcast(), CONSTRUCTOR_ATTR)? {
            self.add_constructor(function_with_body_id, storage_type)?;
        } else if function_with_body_id.has_attr(self.db.upcast(), L1_HANDLER_ATTR)? {
            self.add_l1_handler(function_with_body_id, storage_type)?;
        }
        Ok(())
    }

    /// Adds a function to the ABI.
    fn add_function_with_body(
        &mut self,
        function_with_body_id: FunctionWithBodyId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        let name: String = function_with_body_id.name(self.db.upcast()).into();
        let signature = self.db.function_with_body_signature(function_with_body_id)?;

        let function = self.function_as_abi(&name, signature, storage_type)?;
        self.add_abi_item(Item::Function(function), true, Source::Function(function_with_body_id))?;

        Ok(())
    }

    /// Adds a constructor to the ABI.
    fn add_constructor(
        &mut self,
        function_with_body_id: FunctionWithBodyId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        let source = Source::Function(function_with_body_id);
        if self.ctor.is_some() {
            return Err(ABIError::MultipleConstructors(source));
        }
        let name = function_with_body_id.name(self.db.upcast()).into();
        let signature = self.db.function_with_body_signature(function_with_body_id)?;

        let (inputs, state_mutability) =
            self.get_function_signature_inputs_and_mutability(&signature, storage_type)?;
        self.ctor = Some(EntryPointInfo { source, inputs: inputs.clone() });
        require(state_mutability == StateMutability::External).ok_or(ABIError::UnexpectedType)?;

        let constructor_item = Item::Constructor(Constructor { name, inputs });
        self.add_abi_item(constructor_item, true, source)?;

        Ok(())
    }

    /// Adds an L1 handler to the ABI.
    fn add_l1_handler(
        &mut self,
        function_with_body_id: FunctionWithBodyId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        let name = function_with_body_id.name(self.db.upcast()).into();
        let signature = self.db.function_with_body_signature(function_with_body_id)?;

        let (inputs, state_mutability) =
            self.get_function_signature_inputs_and_mutability(&signature, storage_type)?;

        let outputs = self.get_signature_outputs(&signature)?;

        let l1_handler_item =
            Item::L1Handler(L1Handler { name, inputs, outputs, state_mutability });
        self.add_abi_item(l1_handler_item, true, Source::Function(function_with_body_id))?;

        Ok(())
    }

    /// Inspects a free function and returns its inputs and state mutability.
    fn get_function_signature_inputs_and_mutability(
        &mut self,
        signature: &cairo_lang_semantic::Signature,
        storage_type: TypeId,
    ) -> Result<(Vec<Input>, StateMutability), ABIError> {
        let mut params = signature.params.iter();
        let Some(first_param) = params.next() else {
            return Err(ABIError::EntrypointMustHaveSelf);
        };
        require(first_param.name == "self").ok_or(ABIError::EntrypointMustHaveSelf)?;
        let is_ref = first_param.mutability == Mutability::Reference;
        let expected_storage_ty = is_ref
            .then_some(storage_type)
            .unwrap_or_else(|| TypeLongId::Snapshot(storage_type).intern(self.db));
        require(first_param.ty == expected_storage_ty).ok_or(ABIError::UnexpectedType)?;
        let state_mutability =
            if is_ref { StateMutability::External } else { StateMutability::View };
        let mut inputs = vec![];
        for param in params {
            self.add_type(param.ty)?;
            inputs.push(Input {
                name: param.id.name(self.db.upcast()).into(),
                ty: param.ty.format(self.db),
            });
        }
        Ok((inputs, state_mutability))
    }

    /// Gets the output types of the given signature.
    fn get_signature_outputs(
        &mut self,
        signature: &cairo_lang_semantic::Signature,
    ) -> Result<Vec<Output>, ABIError> {
        // TODO(spapini): output refs?
        Ok(if signature.return_type.is_unit(self.db) {
            vec![]
        } else {
            self.add_type(signature.return_type)?;
            vec![Output { ty: signature.return_type.format(self.db) }]
        })
    }

    /// Converts a TraitFunctionId to an ABI::Function.
    fn trait_function_as_abi(
        &mut self,
        trait_function_id: TraitFunctionId,
        storage_type: TypeId,
    ) -> Result<Function, ABIError> {
        let name: String = trait_function_id.name(self.db.upcast()).into();
        let signature = self.db.trait_function_signature(trait_function_id)?;

        self.function_as_abi(&name, signature, storage_type)
    }

    /// Converts a function name and signature to an ABI::Function.
    fn function_as_abi(
        &mut self,
        name: &str,
        signature: Signature,
        storage_type: TypeId,
    ) -> Result<Function, ABIError> {
        let (inputs, state_mutability) =
            self.get_function_signature_inputs_and_mutability(&signature, storage_type)?;

        let outputs = self.get_signature_outputs(&signature)?;

        Ok(Function { name: name.to_string(), inputs, outputs, state_mutability })
    }

    /// Adds an event to the ABI from a type with an Event derive.
    fn add_event(&mut self, type_id: TypeId, source: Source) -> Result<(), ABIError> {
        if self.event_info.contains_key(&type_id) {
            // The event was handled previously.
            return Ok(());
        }

        let concrete = try_extract_matches!(type_id.lookup_intern(self.db), TypeLongId::Concrete)
            .ok_or(ABIError::UnexpectedType)?;
        let (event_kind, source) = match fetch_event_data(self.db, type_id)
            .ok_or(ABIError::EventNotDerived(source))?
        {
            EventData::Struct { members } => {
                let ConcreteTypeId::Struct(concrete_struct_id) = concrete else {
                    unreachable!();
                };
                let concrete_members = self.db.concrete_struct_members(concrete_struct_id)?;
                let event_fields = members
                    .into_iter()
                    .map(|(name, kind)| {
                        let concrete_member = &concrete_members[&name];
                        let ty = concrete_member.ty;
                        self.add_event_field(kind, ty, name, Source::Member(concrete_member.id))
                    })
                    .collect::<Result<_, ABIError>>()?;
                self.event_info.insert(type_id, EventInfo::Struct);
                (EventKind::Struct { members: event_fields }, Source::Struct(concrete_struct_id))
            }
            EventData::Enum { variants } => {
                let ConcreteTypeId::Enum(concrete_enum_id) = concrete else {
                    unreachable!();
                };
                let mut selectors = HashSet::new();
                let mut add_selector = |selector: &str, source_ptr| {
                    if !selectors.insert(selector.to_string()) {
                        Err(ABIError::EventSelectorDuplication {
                            event: type_id.format(self.db),
                            selector: selector.to_string(),
                            source_ptr,
                        })
                    } else {
                        Ok(())
                    }
                };
                let concrete_variants = self.db.concrete_enum_variants(concrete_enum_id)?;
                let event_fields = zip_eq(variants, concrete_variants)
                    .map(|((name, kind), concrete_variant)| {
                        let source = Source::Variant(concrete_variant.id);
                        if kind == EventFieldKind::Nested {
                            add_selector(&name, source)?;
                        }
                        let field =
                            self.add_event_field(kind, concrete_variant.ty, name.clone(), source)?;
                        if kind == EventFieldKind::Flat {
                            if let EventInfo::Enum(inner) = &self.event_info[&concrete_variant.ty] {
                                for selector in inner {
                                    add_selector(selector, source)?;
                                }
                            } else {
                                let bad_attr = concrete_variant
                                    .concrete_enum_id
                                    .enum_id(self.db)
                                    .stable_ptr(self.db.upcast())
                                    .lookup(self.db.upcast())
                                    .variants(self.db.upcast())
                                    .elements(self.db.upcast())
                                    .into_iter()
                                    .find_map(|v| {
                                        if v.name(self.db.upcast()).text(self.db.upcast()) == name {
                                            v.find_attr(self.db.upcast(), FLAT_ATTR)
                                        } else {
                                            None
                                        }
                                    })
                                    .expect("Impossible mismatch between AuxData and syntax");
                                return Err(ABIError::EventFlatVariantMustBeEnum(bad_attr));
                            }
                        }
                        Ok(field)
                    })
                    .collect::<Result<_, ABIError>>()?;
                self.event_info.insert(type_id, EventInfo::Enum(selectors));
                (EventKind::Enum { variants: event_fields }, Source::Enum(concrete_enum_id))
            }
        };
        let event_item = Item::Event(Event { name: type_id.format(self.db), kind: event_kind });
        self.add_abi_item(event_item, true, source)?;

        Ok(())
    }

    /// Adds an event field to the ABI.
    fn add_event_field(
        &mut self,
        kind: EventFieldKind,
        ty: TypeId,
        name: SmolStr,
        source: Source,
    ) -> Result<EventField, ABIError> {
        match kind {
            EventFieldKind::KeySerde | EventFieldKind::DataSerde => self.add_type(ty)?,
            EventFieldKind::Nested | EventFieldKind::Flat => self.add_event(ty, source)?,
        };
        Ok(EventField { name: name.into(), ty: ty.format(self.db), kind })
    }

    /// Adds a type to the ABI from a TypeId.
    fn add_type(&mut self, type_id: TypeId) -> Result<(), ABIError> {
        if !self.types.insert(type_id) {
            // The type was handled previously.
            return Ok(());
        }

        match type_id.lookup_intern(self.db) {
            TypeLongId::Concrete(concrete) => self.add_concrete_type(concrete),
            TypeLongId::Tuple(inner_types) => {
                for ty in inner_types {
                    self.add_type(ty)?;
                }
                Ok(())
            }
            TypeLongId::Snapshot(ty) => self.add_type(ty),
            TypeLongId::FixedSizeArray { type_id, .. } => {
                self.add_type(type_id)?;
                Ok(())
            }
            TypeLongId::Coupon(_)
            | TypeLongId::GenericParameter(_)
            | TypeLongId::Var(_)
            | TypeLongId::ImplType(_)
            | TypeLongId::Missing(_)
            | TypeLongId::Closure(_) => Err(ABIError::UnexpectedType),
        }
    }

    /// Adds a concrete type and all inner types that it depends on to ABI.
    /// native types are skipped.
    fn add_concrete_type(&mut self, concrete: ConcreteTypeId) -> Result<(), ABIError> {
        // If we have Array<T>, then we might need to add the type T to the ABI.
        for generic_arg in concrete.generic_args(self.db) {
            if let GenericArgumentId::Type(type_id) = generic_arg {
                self.add_type(type_id)?;
            }
        }

        match concrete {
            ConcreteTypeId::Struct(id) => {
                let members = self.add_and_get_struct_members(id)?;
                let struct_item = Item::Struct(Struct { name: concrete.format(self.db), members });
                self.add_abi_item(struct_item, true, Source::Struct(id))?;
            }
            ConcreteTypeId::Enum(id) => {
                let variants = self.add_and_get_enum_variants(id)?;
                let enum_item = Item::Enum(Enum { name: concrete.format(self.db), variants });
                self.add_abi_item(enum_item, true, Source::Enum(id))?;
            }
            ConcreteTypeId::Extern(_) => {}
        }
        Ok(())
    }

    /// Adds the types of struct members to the ABI, and returns them.
    fn add_and_get_struct_members(
        &mut self,
        id: cairo_lang_semantic::ConcreteStructId,
    ) -> Result<Vec<StructMember>, ABIError> {
        self.db
            .concrete_struct_members(id)?
            .iter()
            .map(|(name, member)| {
                self.add_type(member.ty)?;
                Ok(StructMember { name: name.to_string(), ty: member.ty.format(self.db) })
            })
            .collect()
    }

    /// Adds the types of struct variants to the ABI, and returns them.
    fn add_and_get_enum_variants(
        &mut self,
        id: cairo_lang_semantic::ConcreteEnumId,
    ) -> Result<Vec<EnumVariant>, ABIError> {
        let generic_id = id.enum_id(self.db);

        self.db
            .enum_variants(generic_id)?
            .iter()
            .map(|(name, variant_id)| {
                let variant = self.db.concrete_enum_variant(
                    id,
                    &self.db.variant_semantic(generic_id, *variant_id)?,
                )?;
                self.add_type(variant.ty)?;
                Ok(EnumVariant { name: name.to_string(), ty: variant.ty.format(self.db) })
            })
            .collect::<Result<Vec<_>, ABIError>>()
    }

    /// Adds an item to the ABI.
    /// Returns OK on success, or an ABIError on failure.
    fn add_abi_item(
        &mut self,
        item: Item,
        prevent_dups: bool,
        source: Source,
    ) -> Result<(), ABIError> {
        if let Some((name, inputs)) = match &item {
            Item::Function(item) => Some((item.name.to_string(), item.inputs.clone())),
            Item::Constructor(item) => Some((item.name.to_string(), item.inputs.clone())),
            Item::L1Handler(item) => Some((item.name.to_string(), item.inputs.clone())),
            _ => None,
        } {
            self.add_entry_point(name, EntryPointInfo { source, inputs })?;
        }

        self.insert_abi_item(item, prevent_dups.then_some(source))
    }

    /// Inserts an item to the set of items.
    /// Returns OK on success, or an ABIError on failure, e.g. if `prevent_dups` is true but the
    /// item already existed.
    /// This is the only way to insert an item to the ABI, to make sure the caller explicitly
    /// specifies whether duplication is OK or not.
    /// Should not be used directly, but only through `AbiBuilder::add_abi_item`.
    fn insert_abi_item(
        &mut self,
        item: Item,
        prevent_dups: Option<Source>,
    ) -> Result<(), ABIError> {
        let description = match &item {
            Item::Function(item) => format!("Function '{}'", item.name),
            Item::Constructor(item) => format!("Constructor '{}'", item.name),
            Item::L1Handler(item) => format!("L1 Handler '{}'", item.name),
            Item::Event(item) => format!("Event '{}'", item.name),
            Item::Struct(item) => format!("Struct '{}'", item.name),
            Item::Enum(item) => format!("Enum '{}'", item.name),
            Item::Interface(item) => format!("Interface '{}'", item.name),
            Item::Impl(item) => format!("Impl '{}'", item.name),
        };
        let already_existed = !self.abi_items.insert(item);
        if already_existed {
            if let Some(source) = prevent_dups {
                return Err(ABIError::InvalidDuplicatedItem { description, source_ptr: source });
            }
        }

        Ok(())
    }

    /// Adds an entry point name to the set of names, to track unsupported duplication.
    fn add_entry_point(&mut self, name: String, info: EntryPointInfo) -> Result<(), ABIError> {
        let source_ptr = info.source;
        if self.entry_points.insert(name.clone(), info).is_some() {
            return Err(ABIError::DuplicateEntryPointName { name, source_ptr });
        }
        Ok(())
    }
}

/// Checks whether the impl is marked with #[abi(embed_v0)].
fn is_impl_abi_embed(db: &dyn SemanticGroup, imp: ImplDefId) -> Maybe<bool> {
    imp.has_attr_with_arg(db, ABI_ATTR, ABI_ATTR_EMBED_V0_ARG)
}

/// Checks whether the impl is marked with `#[abi(per_item)]`.
fn is_impl_abi_per_item(db: &dyn SemanticGroup, imp: ImplDefId) -> Maybe<bool> {
    imp.has_attr_with_arg(db, ABI_ATTR, ABI_ATTR_PER_ITEM_ARG)
}

/// Fetch the event data for the given type. Returns None if the given event type doesn't derive
/// `starknet::Event` by using the `derive` attribute.
fn fetch_event_data(db: &dyn SemanticGroup, event_type_id: TypeId) -> Option<EventData> {
    let starknet_module = core_submodule(db, "starknet");
    // `starknet::event`.
    let event_module = try_extract_matches!(
        db.module_item_by_name(starknet_module, "event".into()).unwrap().unwrap(),
        ModuleItemId::Submodule
    )?;
    // `starknet::event::Event`.
    let event_trait_id = try_extract_matches!(
        db.module_item_by_name(ModuleId::Submodule(event_module), "Event".into()).unwrap().unwrap(),
        ModuleItemId::Trait
    )?;
    // `starknet::event::Event<ThisEvent>`.
    let concrete_trait_id = ConcreteTraitLongId {
        trait_id: event_trait_id,
        generic_args: vec![GenericArgumentId::Type(event_type_id)],
    }
    .intern(db);
    // The impl of `starknet::event::Event<ThisEvent>`.
    let event_impl =
        get_impl_at_context(db.upcast(), ImplLookupContext::default(), concrete_trait_id, None)
            .ok()?;
    let concrete_event_impl =
        try_extract_matches!(event_impl.lookup_intern(db), ImplLongId::Concrete)?;
    let impl_def_id = concrete_event_impl.impl_def_id(db);

    // Attempt to extract the event data from the aux data from the impl generation.
    let module_file = impl_def_id.module_file_id(db.upcast());
    let all_aux_data = db.module_generated_file_aux_data(module_file.0).ok()?;
    let aux_data = all_aux_data.get(module_file.1.0)?.as_ref()?;
    Some(aux_data.0.as_any().downcast_ref::<StarknetEventAuxData>()?.event_data.clone())
}

#[derive(Error, Debug)]
pub enum ABIError {
    #[error("Semantic error")]
    SemanticError,
    #[error("Event must be an enum.")]
    EventMustBeEnum(Source),
    #[error("`starknet::Event` variant marked with `#[flat]` must be an enum.")]
    EventFlatVariantMustBeEnum(ast::Attribute),
    #[error("Event must have no generic parameters.")]
    EventWithGenericParams(Source),
    #[error("Event type must derive `starknet::Event`.")]
    EventNotDerived(Source),
    #[error("Event `{event}` has duplicate selector `{selector}`.")]
    EventSelectorDuplication { event: String, selector: String, source_ptr: Source },
    #[error("Interfaces must have exactly one generic parameter.")]
    ExpectedOneGenericParam(Source),
    #[error("Contracts must have only one constructor.")]
    MultipleConstructors(Source),
    #[error("Contracts must have a Storage struct.")]
    NoStorage,
    #[error("Contracts must have only one Storage struct.")]
    MultipleStorages(Source),
    #[error("Got unexpected type.")]
    UnexpectedType,
    #[error("Entrypoints must have a self first param.")]
    EntrypointMustHaveSelf,
    #[error("An embedded impl must be an impl of a trait marked with #[starknet::interface].")]
    EmbeddedImplMustBeInterface(Source),
    #[error("Embedded impls must be annotated with #[starknet::embeddable].")]
    EmbeddedImplNotEmbeddable(Source),
    #[error(
        "An impl marked with #[abi(per_item)] can't be of a trait marked with \
         #[starknet::interface].\n    Consider using #[abi(embed_v0)] instead, or use a \
         non-interface trait."
    )]
    ContractInterfaceImplCannotBePerItem(Source),
    #[error(
        "Invalid duplicated item: {description} is used twice in the same contract. This is not \
         supported."
    )]
    InvalidDuplicatedItem { description: String, source_ptr: Source },
    #[error("Duplicate entry point: '{name}'. This is not currently supported.")]
    DuplicateEntryPointName { name: String, source_ptr: Source },
    #[error("Only supported argument for #[starknet::contract] is `account` or nothing.")]
    IllegalContractAttrArgs,
    #[error(
        "`{selector}` is a reserved entry point name for account contracts only (marked with \
         `#[starknet::contract(account)]`)."
    )]
    EntryPointSupportedOnlyOnAccountContract { selector: String, source_ptr: Source },
    #[error("`{selector}` entry point must exist for account contracts.")]
    EntryPointMissingForAccountContract { selector: String },
    #[error("`{VALIDATE_DEPLOY_ENTRY_POINT_SELECTOR}` entry point must match the constructor.")]
    ValidateDeployMismatchingConstructor(Source),
}
impl ABIError {
    pub fn location(&self, db: &dyn SemanticGroup) -> Option<SyntaxStablePtrId> {
        // TODO(orizi): Add more error locations.
        match self {
            ABIError::SemanticError => None,
            ABIError::EventFlatVariantMustBeEnum(attr) => Some(attr.stable_ptr().untyped()),
            ABIError::NoStorage => None,
            ABIError::UnexpectedType => None,
            ABIError::EntrypointMustHaveSelf => None,
            ABIError::EventNotDerived(source)
            | ABIError::EventSelectorDuplication { source_ptr: source, .. }
            | ABIError::EventMustBeEnum(source)
            | ABIError::EventWithGenericParams(source)
            | ABIError::ExpectedOneGenericParam(source)
            | ABIError::MultipleConstructors(source)
            | ABIError::MultipleStorages(source)
            | ABIError::EmbeddedImplMustBeInterface(source)
            | ABIError::EmbeddedImplNotEmbeddable(source)
            | ABIError::ContractInterfaceImplCannotBePerItem(source)
            | ABIError::InvalidDuplicatedItem { source_ptr: source, .. }
            | ABIError::DuplicateEntryPointName { source_ptr: source, .. }
            | ABIError::EntryPointSupportedOnlyOnAccountContract { source_ptr: source, .. }
            | ABIError::ValidateDeployMismatchingConstructor(source) => Some(source.location(db)),
            ABIError::IllegalContractAttrArgs => None,
            ABIError::EntryPointMissingForAccountContract { .. } => None,
        }
    }
}
impl From<DiagnosticAdded> for ABIError {
    fn from(_: DiagnosticAdded) -> Self {
        ABIError::SemanticError
    }
}

/// The source of an ABI item, used for error reporting.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Source {
    Function(FunctionWithBodyId),
    Impl(ImplDefId),
    ImplAlias(ImplAliasId),
    Struct(cairo_lang_semantic::ConcreteStructId),
    Member(cairo_lang_defs::ids::MemberId),
    Enum(cairo_lang_semantic::ConcreteEnumId),
    Variant(cairo_lang_defs::ids::VariantId),
    Trait(TraitId),
}
impl Source {
    fn location(&self, db: &dyn SemanticGroup) -> SyntaxStablePtrId {
        match self {
            Source::Function(id) => id.untyped_stable_ptr(db.upcast()),
            Source::Impl(id) => id.untyped_stable_ptr(db.upcast()),
            Source::ImplAlias(id) => id.untyped_stable_ptr(db.upcast()),
            Source::Struct(id) => id.struct_id(db).untyped_stable_ptr(db.upcast()),
            Source::Member(id) => id.untyped_stable_ptr(db.upcast()),
            Source::Enum(id) => id.enum_id(db).untyped_stable_ptr(db.upcast()),
            Source::Variant(id) => id.untyped_stable_ptr(db.upcast()),
            Source::Trait(id) => id.untyped_stable_ptr(db.upcast()),
        }
    }
}
