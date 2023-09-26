use std::collections::{HashMap, HashSet};

use cairo_lang_defs::ids::{
    FunctionWithBodyId, ImplAliasId, ImplDefId, ImplFunctionId, LanguageElementId, ModuleId,
    ModuleItemId, SubmoduleId, TopLevelLanguageElementId, TraitFunctionId, TraitId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_semantic::corelib::core_submodule;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::attribute::SemanticQueryAttrs;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::imp::{ImplId, ImplLookupContext};
use cairo_lang_semantic::items::structure::SemanticStructEx;
use cairo_lang_semantic::types::{get_impl_at_context, ConcreteEnumLongId, ConcreteStructLongId};
use cairo_lang_semantic::{
    ConcreteTraitLongId, ConcreteTypeId, GenericArgumentId, GenericParam, Mutability, Signature,
    TypeId, TypeLongId,
};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::try_extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::zip_eq;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use thiserror::Error;

use crate::plugin::aux_data::StarkNetEventAuxData;
use crate::plugin::consts::{
    ABI_ATTR, ABI_ATTR_EMBED_V0_ARG, ABI_ATTR_PER_ITEM_ARG, CONSTRUCTOR_ATTR, CONTRACT_STATE_NAME,
    EMBEDDABLE_ATTR, EVENT_ATTR, EVENT_TYPE_NAME, EXTERNAL_ATTR, INTERFACE_ATTR, L1_HANDLER_ATTR,
};
use crate::plugin::events::{EventData, EventFieldKind};

#[cfg(test)]
#[path = "abi_test.rs"]
mod test;

/// Contract ABI.
#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Contract {
    // TODO(spapini): Add storage variables.
    items: OrderedHashSet<Item>,
}
impl Contract {
    pub fn json(&self) -> String {
        serde_json::to_string_pretty(&self).unwrap()
    }

    /// Validates the ABI entry points counts match the expected counts.
    pub fn sanity_check(
        &self,
        expected_external_count: usize,
        expected_l1_handler_count: usize,
        expected_constructor_count: usize,
    ) {
        let trait_fn_count: UnorderedHashMap<_, _> = self
            .items
            .iter()
            .filter_map(|item| {
                let Item::Interface(imp) = item else {
                    return None;
                };
                Some((imp.name.clone(), imp.items.len()))
            })
            .collect();
        let mut external_count = 0;
        let mut l1_handler_count = 0;
        let mut constructor_count = 0;
        for item in &self.items {
            match item {
                Item::Function(_) => external_count += 1,
                Item::L1Handler(_) => l1_handler_count += 1,
                Item::Constructor(_) => constructor_count += 1,
                Item::Impl(imp) => {
                    external_count += trait_fn_count.get(&imp.interface_name).unwrap_or_else(|| {
                        panic!("Interface `{}` not found in ABI.", imp.interface_name)
                    })
                }
                _ => {}
            }
        }
        assert_eq!(external_count, expected_external_count);
        assert_eq!(l1_handler_count, expected_l1_handler_count);
        assert_eq!(constructor_count, expected_constructor_count);
    }

    /// Inserts an item to the set of items.
    /// Returns OK on success, or an ABIError on failure, e.g. if `prevent_dups` is true but the
    /// item already existed.
    /// This is the only way to insert an item to the ABI, to make sure the caller explicitly
    /// specifies whether duplication is OK or not.
    /// Should not be used directly, but only through `AbiBuilder::add_abi_item`.
    fn insert_item(&mut self, item: Item, prevent_dups: bool) -> Result<(), ABIError> {
        let item_description = item.description();
        let already_existed = !self.items.insert(item);
        if already_existed && prevent_dups {
            return Err(ABIError::InvalidDuplicatedItem { description: item_description });
        }

        Ok(())
    }
}

/// Event information.
enum EventInfo {
    /// The event is a struct.
    Struct,
    /// The event is an enum, contains its set of selectors.
    Enum(HashSet<String>),
}

#[derive(Default)]
pub struct AbiBuilder {
    // The constructed ABI.
    abi: Contract,

    /// List of type that were included abi.
    /// Used to avoid redundancy.
    types: HashSet<TypeId>,

    /// A map of events that were included in the abi to their info.
    /// Used to avoid redundancy, as well as preventing enum events from repeating selectors.
    event_info: HashMap<TypeId, EventInfo>,

    /// List of entry point names that were included in the abi.
    /// Used to avoid duplication.
    entry_point_names: HashSet<String>,

    /// The constructor for the contract.
    ctor: Option<FunctionWithBodyId>,
}
impl AbiBuilder {
    /// Creates a Starknet contract ABI from a ModuleId.
    pub fn submodule_as_contract_abi(
        db: &dyn SemanticGroup,
        submodule_id: SubmoduleId,
    ) -> Result<Contract, ABIError> {
        let mut builder = Self::default();
        let module_id = ModuleId::Submodule(submodule_id);

        let mut free_functions = Vec::new();
        let mut enums = Vec::new();
        let mut structs = Vec::new();
        let mut impl_defs = Vec::new();
        let mut impl_aliases = Vec::new();
        for item in &*db.module_items(module_id).unwrap_or_default() {
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
            let struct_name = struct_id.name(db.upcast());
            if struct_name == CONTRACT_STATE_NAME {
                if storage_type.is_some() {
                    return Err(ABIError::MultipleStorages);
                }
                storage_type = Some(db.intern_type(TypeLongId::Concrete(ConcreteTypeId::Struct(
                    db.intern_concrete_struct(ConcreteStructLongId {
                        struct_id,
                        generic_args: vec![],
                    }),
                ))));
            }
            // Forbid a struct named Event.
            if struct_name == EVENT_TYPE_NAME {
                return Err(ABIError::EventMustBeEnum);
            }
        }
        let Some(storage_type) = storage_type else {
            return Err(ABIError::NoStorage);
        };

        // Add impls to ABI.
        for impl_def in impl_defs {
            let is_of_interface = db.impl_def_trait(impl_def)?.has_attr(db, INTERFACE_ATTR)?;
            // TODO(v3): deprecate the external attribute.
            if impl_def.has_attr(db, EXTERNAL_ATTR)? {
                if is_of_interface {
                    builder.add_embedded_impl(db, impl_def, None)?;
                } else {
                    builder.add_non_interface_impl(db, impl_def, storage_type)?;
                }
            } else if is_impl_abi_embed(db, impl_def)? {
                if !is_of_interface {
                    return Err(ABIError::EmbeddedImplMustBeInterface);
                }
                builder.add_embedded_impl(db, impl_def, None)?;
            } else if is_impl_abi_per_item(db, impl_def)? {
                if is_of_interface {
                    return Err(ABIError::ContractInterfaceImplCannotBePerItem);
                }
                builder.add_per_item_impl(db, impl_def, storage_type)?;
            }
        }
        for impl_alias in impl_aliases {
            if impl_alias.has_attr_with_arg(db, ABI_ATTR, ABI_ATTR_EMBED_V0_ARG)? {
                builder.add_embedded_impl_alias(db, impl_alias)?;
            }
        }

        // Add external functions, constructor and L1 handlers to ABI.
        for free_function_id in free_functions {
            builder.maybe_add_function_with_body(
                db,
                FunctionWithBodyId::Free(free_function_id),
                storage_type,
            )?;
        }

        // Add events to ABI.
        for enum_id in enums {
            let enm_name = enum_id.name(db.upcast());
            if enm_name == EVENT_TYPE_NAME && enum_id.has_attr(db.upcast(), EVENT_ATTR)? {
                // Check that the enum has no generic parameters.
                if !db.enum_generic_params(enum_id).unwrap_or_default().is_empty() {
                    return Err(ABIError::EventWithGenericParams);
                }
                // Get the ConcreteEnumId from the EnumId.
                let concrete_enum_id =
                    db.intern_concrete_enum(ConcreteEnumLongId { enum_id, generic_args: vec![] });
                // Get the TypeId of the enum.
                let ty =
                    db.intern_type(TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)));
                builder.add_event(db, ty)?;
            }
        }

        Ok(builder.abi)
    }

    /// Adds an interface to the ABI.
    fn add_interface(&mut self, db: &dyn SemanticGroup, trait_id: TraitId) -> Result<(), ABIError> {
        // Get storage type
        let generic_params = db.trait_generic_params(trait_id)?;
        let [GenericParam::Type(storage_type)] = generic_params.as_slice() else {
            return Err(ABIError::ExpectedOneGenericParam);
        };
        let storage_type = db.intern_type(TypeLongId::GenericParameter(storage_type.id));

        let interface_path = trait_id.full_path(db.upcast());
        let mut items = Vec::new();
        for function in db.trait_functions(trait_id).unwrap_or_default().values() {
            self.add_entry_point_name(function.name(db.upcast()).into())?;
            items.push(self.trait_function_as_abi(db, *function, storage_type)?);
        }

        let interface_item = Item::Interface(Interface { name: interface_path.clone(), items });
        self.add_abi_item(interface_item, true)?;

        Ok(())
    }

    /// Adds the functions of an `external` impl of non-interface trait to the ABI as external
    /// functions.
    fn add_non_interface_impl(
        &mut self,
        db: &dyn SemanticGroup,
        impl_def_id: ImplDefId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        for function in db.impl_functions(impl_def_id).unwrap_or_default().values() {
            let function_abi = self.impl_function_as_abi(db, *function, storage_type)?;
            self.add_abi_item(function_abi, true)?;
        }

        Ok(())
    }

    /// Adds an impl of a starknet interface to the ABI.
    /// `impl_alias_name` can override the given impl name and is used in the ABI if set.
    fn add_embedded_impl(
        &mut self,
        db: &dyn SemanticGroup,
        impl_def_id: ImplDefId,
        impl_alias_name: Option<String>,
    ) -> Result<(), ABIError> {
        let impl_name = impl_def_id.name(db.upcast());

        let trt = db.impl_def_concrete_trait(impl_def_id)?;
        let trt_path = trt.full_path(db);

        let trait_id = trt.trait_id(db);

        let abi_name = impl_alias_name.unwrap_or(impl_name.into());
        let impl_item = Item::Impl(Imp { name: abi_name, interface_name: trt_path });
        self.add_abi_item(impl_item, true)?;
        self.add_interface(db, trait_id)?;

        Ok(())
    }

    /// Adds an embedded impl to the ABI.
    fn add_per_item_impl(
        &mut self,
        db: &dyn SemanticGroup,
        impl_def_id: ImplDefId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        for impl_function_id in db.impl_functions(impl_def_id).unwrap_or_default().values() {
            self.maybe_add_function_with_body(
                db,
                FunctionWithBodyId::Impl(*impl_function_id),
                storage_type,
            )?;
        }
        Ok(())
    }

    /// Adds an embedded impl alias to the ABI.
    fn add_embedded_impl_alias(
        &mut self,
        db: &dyn SemanticGroup,
        impl_alias_id: ImplAliasId,
    ) -> Result<(), ABIError> {
        let impl_def = db.impl_alias_impl_def(impl_alias_id)?;

        // Verify the impl definition has #[starknet::embeddable].
        if !impl_def.has_attr(db, EMBEDDABLE_ATTR)? {
            return Err(ABIError::EmbeddedImplNotEmbeddable);
        }

        // Verify the trait is marked as #[starknet::interface].
        if !db.impl_def_trait(impl_def)?.has_attr(db, INTERFACE_ATTR)? {
            return Err(ABIError::EmbeddedImplMustBeInterface);
        }

        // Add the impl to the ABI.
        self.add_embedded_impl(db, impl_def, Some(impl_alias_id.name(db.upcast()).into()))?;

        Ok(())
    }

    /// Adds a function to the ABI according to its attributes.
    fn maybe_add_function_with_body(
        &mut self,
        db: &dyn SemanticGroup,
        function_with_body_id: FunctionWithBodyId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        if function_with_body_id.has_attr(db.upcast(), EXTERNAL_ATTR)? {
            self.add_function_with_body(db, function_with_body_id, storage_type)?;
        } else if function_with_body_id.has_attr(db.upcast(), CONSTRUCTOR_ATTR)? {
            self.add_constructor(db, function_with_body_id, storage_type)?;
        } else if function_with_body_id.has_attr(db.upcast(), L1_HANDLER_ATTR)? {
            self.add_l1_handler(db, function_with_body_id, storage_type)?;
        }
        Ok(())
    }

    /// Adds a function to the ABI.
    fn add_function_with_body(
        &mut self,
        db: &dyn SemanticGroup,
        function_with_body_id: FunctionWithBodyId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        let name: String = function_with_body_id.name(db.upcast()).into();
        let signature = db.function_with_body_signature(function_with_body_id)?;

        let function = self.function_as_abi(db, &name, signature, storage_type)?;
        self.add_abi_item(function, true)?;

        Ok(())
    }

    /// Adds a constructor to the ABI.
    fn add_constructor(
        &mut self,
        db: &dyn SemanticGroup,
        function_with_body_id: FunctionWithBodyId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        if self.ctor.is_some() {
            return Err(ABIError::MultipleConstructors);
        }
        self.ctor = Some(function_with_body_id);
        let name = function_with_body_id.name(db.upcast()).into();
        let signature = db.function_with_body_signature(function_with_body_id)?;

        let (inputs, state_mutability) =
            self.get_function_signature_inputs_and_mutability(&signature, storage_type, db)?;
        if state_mutability != StateMutability::External {
            return Err(ABIError::UnexpectedType);
        }

        let constructor_item = Item::Constructor(Constructor { name, inputs });
        self.add_abi_item(constructor_item, true)?;

        Ok(())
    }

    /// Adds an L1 handler to the ABI.
    fn add_l1_handler(
        &mut self,
        db: &dyn SemanticGroup,
        function_with_body_id: FunctionWithBodyId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        let name = function_with_body_id.name(db.upcast()).into();
        let signature = db.function_with_body_signature(function_with_body_id)?;

        let (inputs, state_mutability) =
            self.get_function_signature_inputs_and_mutability(&signature, storage_type, db)?;

        let outputs = self.get_signature_outputs(db, &signature)?;

        let l1_handler_item =
            Item::L1Handler(L1Handler { name, inputs, outputs, state_mutability });
        self.add_abi_item(l1_handler_item, true)?;

        Ok(())
    }

    /// Inspects a free function and returns its inputs and state mutability.
    fn get_function_signature_inputs_and_mutability(
        &mut self,
        signature: &cairo_lang_semantic::Signature,
        storage_type: TypeId,
        db: &dyn SemanticGroup,
    ) -> Result<(Vec<Input>, StateMutability), ABIError> {
        let mut params = signature.params.iter();
        let Some(first_param) = params.next() else {
            return Err(ABIError::EntrypointMustHaveSelf);
        };
        if first_param.name != "self" {
            return Err(ABIError::EntrypointMustHaveSelf);
        }
        let is_ref = first_param.mutability == Mutability::Reference;
        if is_ref {
            if first_param.ty != storage_type {
                return Err(ABIError::UnexpectedType);
            }
        } else if first_param.ty != db.intern_type(TypeLongId::Snapshot(storage_type)) {
            return Err(ABIError::UnexpectedType);
        }
        let state_mutability =
            if is_ref { StateMutability::External } else { StateMutability::View };
        let mut inputs = vec![];
        for param in params {
            self.add_type(db, param.ty)?;
            inputs.push(Input { name: param.id.name(db.upcast()).into(), ty: param.ty.format(db) });
        }
        Ok((inputs, state_mutability))
    }

    /// Gets the output types of the given signature.
    fn get_signature_outputs(
        &mut self,
        db: &dyn SemanticGroup,
        signature: &cairo_lang_semantic::Signature,
    ) -> Result<Vec<Output>, ABIError> {
        // TODO(spapini): output refs?
        Ok(if signature.return_type.is_unit(db) {
            vec![]
        } else {
            self.add_type(db, signature.return_type)?;
            vec![Output { ty: signature.return_type.format(db) }]
        })
    }

    /// Converts a TraitFunctionId to an ABI::Function.
    fn trait_function_as_abi(
        &mut self,
        db: &dyn SemanticGroup,
        trait_function_id: TraitFunctionId,
        storage_type: TypeId,
    ) -> Result<Item, ABIError> {
        let name: String = trait_function_id.name(db.upcast()).into();
        let signature = db.trait_function_signature(trait_function_id)?;

        self.function_as_abi(db, &name, signature, storage_type)
    }

    /// Converts a function name and signature to an ABI::Function.
    fn function_as_abi(
        &mut self,
        db: &dyn SemanticGroup,
        name: &str,
        signature: Signature,
        storage_type: TypeId,
    ) -> Result<Item, ABIError> {
        let (inputs, state_mutability) =
            self.get_function_signature_inputs_and_mutability(&signature, storage_type, db)?;

        let outputs = self.get_signature_outputs(db, &signature)?;

        Ok(Item::Function(Function { name: name.to_string(), inputs, outputs, state_mutability }))
    }

    /// Converts a TraitFunctionId to an ABI::Function.
    fn impl_function_as_abi(
        &mut self,
        db: &dyn SemanticGroup,
        impl_function_id: ImplFunctionId,
        storage_type: TypeId,
    ) -> Result<Item, ABIError> {
        let name: String = impl_function_id.name(db.upcast()).into();
        let signature = db.impl_function_signature(impl_function_id)?;

        self.function_as_abi(db, &name, signature, storage_type)
    }

    /// Adds an event to the ABI from a type with an Event derive.
    fn add_event(&mut self, db: &dyn SemanticGroup, type_id: TypeId) -> Result<(), ABIError> {
        if self.event_info.contains_key(&type_id) {
            // The event was handled previously.
            return Ok(());
        }

        let concrete = try_extract_matches!(db.lookup_intern_type(type_id), TypeLongId::Concrete)
            .ok_or(ABIError::UnexpectedType)?;
        let event_kind = match fetch_event_data(db, type_id).ok_or(ABIError::EventNotDerived)? {
            EventData::Struct { members } => {
                let ConcreteTypeId::Struct(concrete_struct_id) = concrete else {
                    unreachable!();
                };
                let concrete_members = db.concrete_struct_members(concrete_struct_id)?;
                let event_fields = members
                    .into_iter()
                    .map(|(name, kind)| {
                        let concrete_member = &concrete_members[name.clone()];
                        let ty = concrete_member.ty;
                        self.add_event_field(db, kind, ty, name)
                    })
                    .collect::<Result<_, ABIError>>()?;
                self.event_info.insert(type_id, EventInfo::Struct);
                EventKind::Struct { members: event_fields }
            }
            EventData::Enum { variants } => {
                let ConcreteTypeId::Enum(concrete_enum_id) = concrete else {
                    unreachable!();
                };
                let mut selectors = HashSet::new();
                let mut add_selector = |selector: &str| {
                    if !selectors.insert(selector.to_string()) {
                        Err(ABIError::EventSelectorDuplication {
                            event: type_id.format(db),
                            selector: selector.to_string(),
                        })
                    } else {
                        Ok(())
                    }
                };
                let concrete_variants = db.concrete_enum_variants(concrete_enum_id)?;
                let event_fields = zip_eq(variants, concrete_variants)
                    .map(|((name, kind), concrete_variant)| {
                        if kind == EventFieldKind::Nested {
                            add_selector(&name)?;
                        }
                        let field = self.add_event_field(db, kind, concrete_variant.ty, name)?;
                        if kind == EventFieldKind::Flat {
                            if let EventInfo::Enum(inner) = &self.event_info[&concrete_variant.ty] {
                                for selector in inner {
                                    add_selector(selector)?;
                                }
                            } else {
                                return Err(ABIError::EventFlatVariantMustBeEnum);
                            }
                        }
                        Ok(field)
                    })
                    .collect::<Result<_, ABIError>>()?;
                self.event_info.insert(type_id, EventInfo::Enum(selectors));
                EventKind::Enum { variants: event_fields }
            }
        };
        let event_item = Item::Event(Event { name: type_id.format(db), kind: event_kind });
        self.add_abi_item(event_item, true)?;

        Ok(())
    }

    /// Adds an event field to the ABI.
    fn add_event_field(
        &mut self,
        db: &dyn SemanticGroup,
        kind: EventFieldKind,
        ty: TypeId,
        name: SmolStr,
    ) -> Result<EventField, ABIError> {
        match kind {
            EventFieldKind::KeySerde | EventFieldKind::DataSerde => self.add_type(db, ty)?,
            EventFieldKind::Nested | EventFieldKind::Flat => self.add_event(db, ty)?,
        };
        Ok(EventField { name: name.into(), ty: ty.format(db), kind })
    }

    /// Adds a type to the ABI from a TypeId.
    fn add_type(&mut self, db: &dyn SemanticGroup, type_id: TypeId) -> Result<(), ABIError> {
        if !self.types.insert(type_id) {
            // The type was handled previously.
            return Ok(());
        }

        match db.lookup_intern_type(type_id) {
            TypeLongId::Concrete(concrete) => self.add_concrete_type(db, concrete),
            TypeLongId::Tuple(inner_types) => {
                for ty in inner_types {
                    self.add_type(db, ty)?;
                }
                Ok(())
            }
            TypeLongId::Snapshot(ty) => self.add_type(db, ty),
            TypeLongId::GenericParameter(_) | TypeLongId::Var(_) | TypeLongId::Missing(_) => {
                Err(ABIError::UnexpectedType)
            }
        }
    }

    /// Adds a concrete type and all inner types that it depends on to ABI.
    /// native types are skipped.
    fn add_concrete_type(
        &mut self,
        db: &dyn SemanticGroup,
        concrete: ConcreteTypeId,
    ) -> Result<(), ABIError> {
        // If we have Array<T>, then we might need to add the type T to the ABI.
        for generic_arg in concrete.generic_args(db) {
            if let GenericArgumentId::Type(type_id) = generic_arg {
                self.add_type(db, type_id)?;
            }
        }

        match concrete {
            ConcreteTypeId::Struct(id) => {
                let members = self.add_and_get_struct_members(db, id)?;
                let struct_item = Item::Struct(Struct { name: concrete.format(db), members });
                self.add_abi_item(struct_item, true)?;
            }
            ConcreteTypeId::Enum(id) => {
                let variants = self.add_and_get_enum_variants(db, id)?;
                let enum_item = Item::Enum(Enum { name: concrete.format(db), variants });
                self.add_abi_item(enum_item, true)?;
            }
            ConcreteTypeId::Extern(_) => {}
        }
        Ok(())
    }

    /// Adds the types of struct members to the ABI, and returns them.
    fn add_and_get_struct_members(
        &mut self,
        db: &dyn SemanticGroup,
        id: cairo_lang_semantic::ConcreteStructId,
    ) -> Result<Vec<StructMember>, ABIError> {
        db.concrete_struct_members(id)?
            .iter()
            .map(|(name, member)| {
                self.add_type(db, member.ty)?;
                Ok(StructMember { name: name.to_string(), ty: member.ty.format(db) })
            })
            .collect()
    }

    /// Adds the types of struct variants to the ABI, and returns them.
    fn add_and_get_enum_variants(
        &mut self,
        db: &dyn SemanticGroup,
        id: cairo_lang_semantic::ConcreteEnumId,
    ) -> Result<Vec<EnumVariant>, ABIError> {
        let generic_id = id.enum_id(db);

        db.enum_variants(generic_id)?
            .iter()
            .map(|(name, variant_id)| {
                let variant =
                    db.concrete_enum_variant(id, &db.variant_semantic(generic_id, *variant_id)?)?;
                self.add_type(db, variant.ty)?;
                Ok(EnumVariant { name: name.to_string(), ty: variant.ty.format(db) })
            })
            .collect::<Result<Vec<_>, ABIError>>()
    }

    /// Adds an item to the ABI.
    /// Returns OK on success, or an ABIError on failure.
    fn add_abi_item(&mut self, item: Item, prevent_dups: bool) -> Result<(), ABIError> {
        if let Some(name) = match &item {
            Item::Function(item) => Some(item.name.to_string()),
            Item::Constructor(item) => Some(item.name.to_string()),
            Item::L1Handler(item) => Some(item.name.to_string()),
            _ => None,
        } {
            self.add_entry_point_name(name)?;
        }

        self.abi.insert_item(item, prevent_dups)
    }

    /// Adds an entry point name to the set of names, to track unsupported duplication.
    fn add_entry_point_name(&mut self, name: String) -> Result<(), ABIError> {
        if !self.entry_point_names.insert(name.clone()) {
            return Err(ABIError::DuplicateEntryPointName { name });
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
    let concrete_trait_id = db.intern_concrete_trait(ConcreteTraitLongId {
        trait_id: event_trait_id,
        generic_args: vec![GenericArgumentId::Type(event_type_id)],
    });
    // The impl of `starknet::event::Event<ThisEvent>`.
    let event_impl =
        get_impl_at_context(db.upcast(), ImplLookupContext::default(), concrete_trait_id, None)
            .ok()?;
    let concrete_event_impl = try_extract_matches!(event_impl, ImplId::Concrete)?;
    let impl_def_id = concrete_event_impl.impl_def_id(db);

    // Attempt to extract the event data from the aux data from the impl generation.
    let module_file = impl_def_id.module_file_id(db.upcast());
    let file_infos = db.module_generated_file_infos(module_file.0).ok()?;
    let aux_data = file_infos.get(module_file.1.0)?.as_ref()?.aux_data.as_ref()?;
    Some(aux_data.0.as_any().downcast_ref::<StarkNetEventAuxData>()?.event_data.clone())
}

#[derive(Error, Debug)]
pub enum ABIError {
    #[error("Semantic error")]
    SemanticError,
    #[error("Event must be an enum.")]
    EventMustBeEnum,
    #[error("`starknet::Event` variant marked with `#[flat]` must be an enum.")]
    EventFlatVariantMustBeEnum,
    #[error("Event must have no generic parameters.")]
    EventWithGenericParams,
    #[error("Event type must derive `starknet::Event`.")]
    EventNotDerived,
    #[error("Event `{event}` has duplicate selector `{selector}`.")]
    EventSelectorDuplication { event: String, selector: String },
    #[error("Interfaces must have exactly one generic parameter.")]
    ExpectedOneGenericParam,
    #[error("Contracts must have only one constructor.")]
    MultipleConstructors,
    #[error("Contracts must have a Storage struct.")]
    NoStorage,
    #[error("Contracts must have only one Storage struct.")]
    MultipleStorages,
    #[error("Compilation error.")]
    CompilationError,
    #[error("Got unexpected type.")]
    UnexpectedType,
    #[error("Entrypoints must have a self first param.")]
    EntrypointMustHaveSelf,
    #[error("Entrypoint attribute must match the mutability of the self parameter")]
    AttributeMismatch,
    #[error("An embedded impl must be an impl of a trait marked with #[starknet::interface].")]
    EmbeddedImplMustBeInterface,
    #[error("Embedded impls must be annotated with #[starknet::embeddable].")]
    EmbeddedImplNotEmbeddable,
    #[error("The first generic parameter of an embedded impl must be `TContractState`.")]
    WrongEmbeddedImplFirstGeneric,
    #[error("Only the first generic parameter of an embeddable impl can be a type.")]
    EmbeddableImplWithExtraGenerics,
    #[error(
        "An impl marked with #[abi(per_item)] can't be of a trait marked with \
         #[starknet::interface]. Consider using #[abi(embed_v0)] instead, or use a non-interface \
         trait."
    )]
    ContractInterfaceImplCannotBePerItem,
    #[error(
        "Invalid duplicated item: {description} is used twice in the same contract. This is not \
         supported."
    )]
    InvalidDuplicatedItem { description: String },
    #[error("Duplicate entry point name: {name}. This is not currently supported.")]
    DuplicateEntryPointName { name: String },
}
impl From<DiagnosticAdded> for ABIError {
    fn from(_: DiagnosticAdded) -> Self {
        ABIError::SemanticError
    }
}

/// Enum of contract item ABIs.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[serde(tag = "type")]
pub enum Item {
    #[serde(rename = "function")]
    Function(Function),
    #[serde(rename = "constructor")]
    Constructor(Constructor),
    #[serde(rename = "l1_handler")]
    L1Handler(L1Handler),
    #[serde(rename = "event")]
    Event(Event),
    #[serde(rename = "struct")]
    Struct(Struct),
    #[serde(rename = "enum")]
    Enum(Enum),
    #[serde(rename = "interface")]
    Interface(Interface),
    #[serde(rename = "impl")]
    Impl(Imp),
}
impl Item {
    fn description(&self) -> String {
        match self {
            Item::Function(item) => format!("Function {}", item.name),
            Item::Constructor(item) => format!("Constructor {}", item.name),
            Item::L1Handler(item) => format!("L1 Handler {}", item.name),
            Item::Event(item) => format!("Event {}", item.name),
            Item::Struct(item) => format!("Struct {}", item.name),
            Item::Enum(item) => format!("Enum {}", item.name),
            Item::Interface(item) => format!("Interface {}", item.name),
            Item::Impl(item) => format!("Impl {}", item.name),
        }
    }
}

/// Contract interface ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Interface {
    pub name: String,
    pub items: Vec<Item>,
}

/// Contract impl ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Imp {
    pub name: String,
    pub interface_name: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum StateMutability {
    #[serde(rename = "external")]
    External,
    #[serde(rename = "view")]
    View,
}

/// Contract function ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Function {
    pub name: String,
    pub inputs: Vec<Input>,

    // TODO(ilya): Should the output be a vector or a single type?
    pub outputs: Vec<Output>,
    pub state_mutability: StateMutability,
}

/// Contract constructor ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Constructor {
    pub name: String,
    pub inputs: Vec<Input>,
}

/// Contract L1 handler ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct L1Handler {
    pub name: String,
    pub inputs: Vec<Input>,

    // TODO(ilya): Should the output be a vector or a single type?
    pub outputs: Vec<Output>,
    pub state_mutability: StateMutability,
}

/// Contract event.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Event {
    pub name: String,
    #[serde(flatten)]
    pub kind: EventKind,
}

/// Contract event kind.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[serde(tag = "kind")]
pub enum EventKind {
    #[serde(rename = "struct")]
    Struct { members: Vec<EventField> },
    #[serde(rename = "enum")]
    Enum { variants: Vec<EventField> },
}

/// Contract event field (member/variant).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct EventField {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
    pub kind: EventFieldKind,
}

/// Function input ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Input {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
}

/// Function Output ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Output {
    #[serde(rename = "type")]
    pub ty: String,
}

/// Struct ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Struct {
    pub name: String,
    pub members: Vec<StructMember>,
}

/// Struct member.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct StructMember {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
}

/// Enum ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

/// Enum variant.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct EnumVariant {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
}
