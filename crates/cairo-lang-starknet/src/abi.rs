use std::collections::{HashMap, HashSet};

use cairo_lang_defs::ids::{
    FreeFunctionId, ImplDefId, ImplFunctionId, LanguageElementId, ModuleId, SubmoduleId,
    TopLevelLanguageElementId, TraitFunctionId, TraitId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::structure::SemanticStructEx;
use cairo_lang_semantic::plugin::DynPluginAuxData;
use cairo_lang_semantic::types::{ConcreteEnumLongId, ConcreteStructLongId};
use cairo_lang_semantic::{
    ConcreteTypeId, GenericArgumentId, GenericParam, Mutability, TypeId, TypeLongId,
};
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::Terminal;
use cairo_lang_utils::{extract_matches, try_extract_matches};
use itertools::zip_eq;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use thiserror::Error;

use crate::plugin::aux_data::StarkNetEventAuxData;
use crate::plugin::consts::{CONSTRUCTOR_ATTR, EXTERNAL_ATTR, INTERFACE_ATTR, L1_HANDLER_ATTR};
use crate::plugin::events::{EventData, EventFieldKind};

#[cfg(test)]
#[path = "abi_test.rs"]
mod test;

/// Contract ABI.
#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Contract {
    // TODO(spapini): Add storage variables.
    pub items: Vec<Item>,
}
impl Contract {
    pub fn json(&self) -> String {
        serde_json::to_string_pretty(&self).unwrap()
    }
}

#[derive(Default)]
pub struct AbiBuilder {
    // The constructed ABI.
    abi: Contract,

    /// List of type that were included abi.
    /// Used to avoid redundancy.
    types: HashSet<TypeId>,

    /// List of events that were included in the abi.
    /// Used to avoid redundancy.
    events: HashSet<TypeId>,

    /// Mapping from an event type to its derive plugin emitted data.
    event_derive_data: HashMap<TypeId, EventData>,
}

impl AbiBuilder {
    /// Creates a Starknet contract ABI from a ModuleId.
    pub fn submodule_as_contract_abi(
        db: &dyn SemanticGroup,
        submodule_id: SubmoduleId,
    ) -> Result<Contract, ABIError> {
        let mut builder = Self::default();
        let module_id = ModuleId::Submodule(submodule_id);

        // Get storage type for later validations.
        // TODO(yuval): This is a temporary measure to find the Storage type. Instead, get
        // storage type from aux data from the plugin.
        let mut storage_type = None;
        for (id, strct) in db.module_structs(module_id).unwrap_or_default() {
            if strct.name(db.upcast()).text(db.upcast()) == "Storage" {
                if storage_type.is_some() {
                    return Err(ABIError::MultipleStorages);
                }
                storage_type = Some(db.intern_type(TypeLongId::Concrete(ConcreteTypeId::Struct(
                    db.intern_concrete_struct(ConcreteStructLongId {
                        struct_id: id,
                        generic_args: vec![],
                    }),
                ))));
            }
            // Forbid a struct named Event.
            if strct.name(db.upcast()).text(db.upcast()) == "Event" {
                return Err(ABIError::EventMustBeEnum);
            }
        }
        let Some(storage_type) = storage_type else {
            return Err(ABIError::NoStorage);
        };

        // Add impls to ABI.
        for (id, imp) in db.module_impls(module_id).unwrap_or_default() {
            if imp.has_attr(db.upcast(), EXTERNAL_ATTR) {
                builder.add_impl(db, id, storage_type)?;
                continue;
            }
            // Check if we have an Event derive plugin data on the impl.
            let module_file = id.module_file_id(db.upcast());
            let generate_info =
                db.module_generated_file_infos(module_file.0)?[module_file.1.0].clone();
            let Some(generate_info) = generate_info else { continue };
            let Some(mapper) = generate_info.aux_data.0.as_any(
                    ).downcast_ref::<DynPluginAuxData>() else { continue; };
            let Some(aux_data) = mapper.0.as_any(
                    ).downcast_ref::<StarkNetEventAuxData>() else { continue; };
            let concrete_trait_id = db.impl_def_concrete_trait(id)?;
            let ty =
                extract_matches!(concrete_trait_id.generic_args(db)[0], GenericArgumentId::Type);
            builder.event_derive_data.insert(ty, aux_data.event_data.clone());
        }

        // Add external functions, constructor and L1 handlers to ABI.
        let mut ctor = None;
        for (id, function) in db.module_free_functions(module_id).unwrap_or_default() {
            if function.has_attr(db.upcast(), EXTERNAL_ATTR) {
                builder.add_free_function(db, id, storage_type)?;
            } else if function.has_attr(db.upcast(), CONSTRUCTOR_ATTR) {
                if ctor.is_some() {
                    return Err(ABIError::MultipleConstructors);
                } else {
                    ctor = Some(id);
                    builder.add_constructor(db, id, storage_type)?;
                }
            } else if function.has_attr(db.upcast(), L1_HANDLER_ATTR) {
                builder.add_l1_handler(db, id, storage_type)?;
            }
        }

        // Add events.
        for (id, enm) in db.module_enums(module_id).unwrap_or_default() {
            // TODO(yuval): Enforce an event attr.
            if enm.name(db.upcast()).text(db.upcast()) == "Event" {
                // Check that the enum has no generic parameters.
                if !db.enum_generic_params(id).unwrap_or_default().is_empty() {
                    return Err(ABIError::EventWithGenericParams);
                }
                // Get the ConcreteEnumId from the EnumId.
                let concrete_enum_id = db
                    .intern_concrete_enum(ConcreteEnumLongId { enum_id: id, generic_args: vec![] });
                // Get the TypeId of the enum.
                let ty =
                    db.intern_type(TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)));
                builder.add_event(db, ty)?;
            }
        }

        Ok(builder.abi)
    }

    // TODO(yuval): Remove, this was for the old syntax (remove add_trait_function, add_event and
    // trait_function_has_attr as well).
    /// Creates a Starknet contract ABI from a TraitId.
    pub fn trait_as_interface_abi(
        db: &dyn SemanticGroup,
        trait_id: TraitId,
    ) -> Result<Contract, ABIError> {
        let generic_params =
            db.trait_generic_params(trait_id).map_err(|_| ABIError::CompilationError)?;
        let [GenericParam::Type(storage_type)] = generic_params.as_slice() else {
            return Err(ABIError::ExpectedOneGenericParam);
        };
        let storage_type = db.intern_type(TypeLongId::GenericParameter(storage_type.id));

        let mut builder = Self::default();

        for trait_function_id in db.trait_functions(trait_id).unwrap_or_default().values() {
            builder.add_trait_function(db, *trait_function_id, storage_type)?;
        }

        Ok(builder.abi)
    }

    /// Adds an interface to the ABI.
    fn add_interface(&mut self, db: &dyn SemanticGroup, trait_id: TraitId) -> Result<(), ABIError> {
        // Get storage type
        let generic_params =
            db.trait_generic_params(trait_id).map_err(|_| ABIError::CompilationError)?;
        let [GenericParam::Type(storage_type)] = generic_params.as_slice() else {
            return Err(ABIError::ExpectedOneGenericParam);
        };
        let storage_type = db.intern_type(TypeLongId::GenericParameter(storage_type.id));

        let interface_path = trait_id.full_path(db.upcast());
        let mut items = Vec::new();
        for function in db.trait_functions(trait_id).unwrap_or_default().values() {
            items.push(Item::Function(self.trait_function_as_abi(db, *function, storage_type)?));
        }

        self.abi.items.push(Item::Interface(Interface { name: interface_path, items }));

        Ok(())
    }

    /// Adds the functions of a non-interface impl to the ABI as external functions.
    /// A non-interface impl is an impl whose trait is not marked as `#[starknet::interface]`
    fn add_non_interface_impl(
        &mut self,
        db: &dyn SemanticGroup,
        impl_def_id: ImplDefId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        for function in db.impl_functions(impl_def_id).unwrap_or_default().values() {
            let function_abi =
                Item::Function(self.impl_function_as_abi(db, *function, storage_type)?);
            self.abi.items.push(function_abi);
        }

        Ok(())
    }

    /// Adds an impl to the ABI.
    fn add_impl(
        &mut self,
        db: &dyn SemanticGroup,
        impl_def_id: ImplDefId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        let impl_name = impl_def_id.name(db.upcast());

        let trt =
            db.impl_def_concrete_trait(impl_def_id).map_err(|_| ABIError::CompilationError)?;
        let trt_path = trt.full_path(db);

        // If the trait is marked as starknet::interface, add the interface. Otherwise, add the
        // functions as external functions.
        let trait_id = trt.trait_id(db);
        let attrs = db.trait_attributes(trait_id).map_err(|_| ABIError::CompilationError)?;
        if attrs.into_iter().any(|x| x.id == INTERFACE_ATTR) {
            self.abi
                .items
                .push(Item::Impl(Imp { name: impl_name.into(), interface_name: trt_path }));
            self.add_interface(db, trait_id)?;
        } else {
            self.add_non_interface_impl(db, impl_def_id, storage_type)?;
        }

        Ok(())
    }

    /// Adds a free function to the ABI.
    fn add_free_function(
        &mut self,
        db: &dyn SemanticGroup,
        free_function_id: FreeFunctionId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        let name: String = free_function_id.name(db.upcast()).into();
        let signature =
            db.free_function_signature(free_function_id).map_err(|_| ABIError::CompilationError)?;

        let (inputs, state_mutability) =
            self.get_function_signature_inputs_and_mutability(&signature, storage_type, db)?;

        let outputs = self.get_signature_outputs(db, &signature)?;

        self.abi.items.push(Item::Function(Function { name, inputs, outputs, state_mutability }));

        Ok(())
    }

    /// Adds a constructor to the ABI.
    fn add_constructor(
        &mut self,
        db: &dyn SemanticGroup,
        free_function_id: FreeFunctionId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        let name = free_function_id.name(db.upcast()).into();
        let signature =
            db.free_function_signature(free_function_id).map_err(|_| ABIError::CompilationError)?;

        let (inputs, state_mutability) =
            self.get_function_signature_inputs_and_mutability(&signature, storage_type, db)?;
        if state_mutability != StateMutability::External {
            return Err(ABIError::UnexpectedType);
        }

        self.abi.items.push(Item::Constructor(Constructor { name, inputs }));

        Ok(())
    }

    /// Adds an L1 handler to the ABI.
    fn add_l1_handler(
        &mut self,
        db: &dyn SemanticGroup,
        free_function_id: FreeFunctionId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        let name = free_function_id.name(db.upcast()).into();
        let signature =
            db.free_function_signature(free_function_id).map_err(|_| ABIError::CompilationError)?;

        let (inputs, state_mutability) =
            self.get_function_signature_inputs_and_mutability(&signature, storage_type, db)?;

        let outputs = self.get_signature_outputs(db, &signature)?;

        self.abi.items.push(Item::L1Handler(L1Handler { name, inputs, outputs, state_mutability }));

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
    ) -> Result<Function, ABIError> {
        let name = trait_function_id.name(db.upcast()).into();
        let signature = db
            .trait_function_signature(trait_function_id)
            .map_err(|_| ABIError::CompilationError)?;

        let (inputs, state_mutability) =
            self.get_function_signature_inputs_and_mutability(&signature, storage_type, db)?;

        let outputs = self.get_signature_outputs(db, &signature)?;

        Ok(Function { name, inputs, outputs, state_mutability })
    }

    /// Converts a TraitFunctionId to an ABI::Function.
    fn impl_function_as_abi(
        &mut self,
        db: &dyn SemanticGroup,
        impl_function_id: ImplFunctionId,
        storage_type: TypeId,
    ) -> Result<Function, ABIError> {
        let name = impl_function_id.name(db.upcast()).into();
        let signature =
            db.impl_function_signature(impl_function_id).map_err(|_| ABIError::CompilationError)?;

        let (inputs, state_mutability) =
            self.get_function_signature_inputs_and_mutability(&signature, storage_type, db)?;

        let outputs = self.get_signature_outputs(db, &signature)?;

        Ok(Function { name, inputs, outputs, state_mutability })
    }

    /// Adds a function to the ABI from a TraitFunctionId.
    fn add_trait_function(
        &mut self,
        db: &dyn SemanticGroup,
        trait_function_id: TraitFunctionId,
        storage_type: TypeId,
    ) -> Result<(), ABIError> {
        let function = self.trait_function_as_abi(db, trait_function_id, storage_type)?;
        self.abi.items.push(Item::Function(function));

        Ok(())
    }

    /// Adds an event to the ABI from a type with an Event derive.
    fn add_event(&mut self, db: &dyn SemanticGroup, type_id: TypeId) -> Result<(), ABIError> {
        if !self.events.insert(type_id) {
            // The event was handled previously.
            return Ok(());
        }

        let TypeLongId::Concrete(concrete) = db.lookup_intern_type(type_id) else {
            return Err(ABIError::UnexpectedType);
        };
        let Some(event_data) = self.event_derive_data.get(&type_id) else {
            return Err(ABIError::EventNotDerived)
        };

        let event_kind = match event_data.clone() {
            EventData::Struct { members } => {
                let ConcreteTypeId::Struct(concrete_struct_id) = concrete else { unreachable!(); };
                let concrete_members = db.concrete_struct_members(concrete_struct_id)?;
                let event_fields = members
                    .into_iter()
                    .map(|(name, kind)| {
                        let concrete_member = &concrete_members[name.clone()];
                        let ty = concrete_member.ty;
                        self.add_event_field(db, kind, ty, name)
                    })
                    .collect::<Result<_, ABIError>>()?;
                EventKind::Struct { members: event_fields }
            }
            EventData::Enum { variants } => {
                let ConcreteTypeId::Enum(concrete_enum_id) = concrete else { unreachable!(); };
                let concrete_variants = db.concrete_enum_variants(concrete_enum_id)?;
                let event_fields = zip_eq(variants, concrete_variants)
                    .map(|((name, kind), concrete_variant)| {
                        let ty = concrete_variant.ty;

                        let ty_name = get_type_name(db, ty).unwrap_or_default();
                        if name != ty_name {
                            return Err(ABIError::EventEnumVariantTypeMismatch);
                        }

                        self.add_event_field(db, kind, ty, name)
                    })
                    .collect::<Result<_, ABIError>>()?;
                EventKind::Enum { variants: event_fields }
            }
        };
        let name = type_id.format(db);
        self.abi.items.push(Item::Event(Event { name, kind: event_kind }));

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
            EventFieldKind::KeySerde | EventFieldKind::ValueSerde => self.add_type(db, ty)?,
            EventFieldKind::Nested => self.add_event(db, ty)?,
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

        if is_native_type(db, &concrete) {
            return Ok(());
        }

        match concrete {
            ConcreteTypeId::Struct(id) => self.abi.items.push(Item::Struct(Struct {
                name: concrete.format(db),
                members: get_struct_members(db, id).map_err(|_| ABIError::UnexpectedType)?,
            })),
            ConcreteTypeId::Enum(id) => self.abi.items.push(Item::Enum(Enum {
                name: concrete.format(db),
                variants: get_enum_variants(db, id).map_err(|_| ABIError::UnexpectedType)?,
            })),
            ConcreteTypeId::Extern(_) => {}
        }
        Ok(())
    }
}

fn get_type_name(db: &dyn SemanticGroup, ty: TypeId) -> Option<SmolStr> {
    let concrete_ty = try_extract_matches!(db.lookup_intern_type(ty), TypeLongId::Concrete)?;
    Some(concrete_ty.generic_type(db).name(db.upcast()))
}

fn get_struct_members(
    db: &dyn SemanticGroup,
    id: cairo_lang_semantic::ConcreteStructId,
) -> Maybe<Vec<StructMember>> {
    Ok(db
        .concrete_struct_members(id)?
        .iter()
        .map(|(name, member)| StructMember { name: name.to_string(), ty: member.ty.format(db) })
        .collect())
}

fn get_enum_variants(
    db: &dyn SemanticGroup,
    id: cairo_lang_semantic::ConcreteEnumId,
) -> Maybe<Vec<EnumVariant>> {
    let generic_id = id.enum_id(db);

    db.enum_variants(generic_id)?
        .iter()
        .map(|(name, variant_id)| {
            Ok(EnumVariant {
                name: name.to_string(),
                ty: db
                    .concrete_enum_variant(id, &db.variant_semantic(generic_id, *variant_id)?)?
                    .ty
                    .format(db),
            })
        })
        .collect::<Result<Vec<_>, DiagnosticAdded>>()
}

/// Returns true if concrete is a native type.
///
/// native types are not added to the ABI.
fn is_native_type(db: &dyn SemanticGroup, concrete: &ConcreteTypeId) -> bool {
    let def_db = db.upcast();
    concrete.generic_type(db).parent_module(def_db).owning_crate(def_db) == db.core_crate()
}

#[derive(Error, Debug)]
pub enum ABIError {
    #[error("Semantic error")]
    SemanticError,
    #[error("Event enum variant type must be a struct with the same name as the variant.")]
    EventEnumVariantTypeMismatch,
    #[error("Event must be an enum.")]
    EventMustBeEnum,
    #[error("Event must have no generic parameters.")]
    EventWithGenericParams,
    #[error("Event type must derive `starknet::Event`.")]
    EventNotDerived,
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
}
impl From<DiagnosticAdded> for ABIError {
    fn from(_: DiagnosticAdded) -> Self {
        ABIError::SemanticError
    }
}

/// Enum of contract item ABIs.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
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

/// Contract interface ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Interface {
    pub name: String,
    pub items: Vec<Item>,
}

/// Contract impl ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Imp {
    pub name: String,
    pub interface_name: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum StateMutability {
    #[serde(rename = "external")]
    External,
    #[serde(rename = "view")]
    View,
}

/// Contract function ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub inputs: Vec<Input>,

    // TODO(ilya): Should the output be a vector or a single type?
    pub outputs: Vec<Output>,
    pub state_mutability: StateMutability,
}

/// Contract constructor ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Constructor {
    pub name: String,
    pub inputs: Vec<Input>,
}

/// Contract L1 handler ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct L1Handler {
    pub name: String,
    pub inputs: Vec<Input>,

    // TODO(ilya): Should the output be a vector or a single type?
    pub outputs: Vec<Output>,
    pub state_mutability: StateMutability,
}

/// Contract event.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Event {
    pub name: String,
    #[serde(flatten)]
    pub kind: EventKind,
}

/// Contract event kind.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum EventKind {
    #[serde(rename = "struct")]
    Struct { members: Vec<EventField> },
    #[serde(rename = "enum")]
    Enum { variants: Vec<EventField> },
}

/// Contract event field (member/variant).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EventField {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
    pub kind: EventFieldKind,
}

/// Function input ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Input {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
}

/// Function Output ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Output {
    #[serde(rename = "type")]
    pub ty: String,
}

/// Struct ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Struct {
    pub name: String,
    pub members: Vec<StructMember>,
}

/// Struct member.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructMember {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
}

/// Enum ABI.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

/// Enum variant.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnumVariant {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
}
