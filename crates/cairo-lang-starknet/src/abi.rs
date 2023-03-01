use std::collections::HashSet;

use cairo_lang_defs::ids::{LanguageElementId, TraitFunctionId, TraitId};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::structure::SemanticStructEx;
use cairo_lang_semantic::{ConcreteTypeId, TypeId, TypeLongId};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::plugin::consts::{EVENT_ATTR, VIEW_ATTR};

#[cfg(test)]
#[path = "abi_test.rs"]
mod test;

/// Contract ABI.
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
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

pub struct AbiBuilder {
    // The constructed ABI.
    abi: Contract,

    /// List of type that were included abi.
    /// Used to avoid redendency.
    types: HashSet<TypeId>,
}

impl AbiBuilder {
    /// Creates a Starknet contract ABI from a TraitId.
    pub fn from_trait(db: &dyn SemanticGroup, trait_id: TraitId) -> Result<Contract, ABIError> {
        if !db.trait_generic_params(trait_id).map_err(|_| ABIError::CompilationError)?.is_empty() {
            return Err(ABIError::GenericTraitsUnsupported);
        }

        let mut builder = Self { abi: Contract::default(), types: HashSet::new() };

        for trait_function_id in db.trait_functions(trait_id).unwrap_or_default().values() {
            if trait_function_has_attr(db, *trait_function_id, EVENT_ATTR)? {
                builder.add_event(db, *trait_function_id)?;
            } else {
                builder.add_function(db, *trait_function_id)?;
            }
        }

        Ok(builder.abi)
    }

    /// Adds a function to the ABI from a TraitFunctionId.
    fn add_function(
        &mut self,
        db: &dyn SemanticGroup,
        trait_function_id: TraitFunctionId,
    ) -> Result<(), ABIError> {
        let state_mutability = if trait_function_has_attr(db, trait_function_id, VIEW_ATTR)? {
            StateMutability::View
        } else {
            StateMutability::External
        };
        let defs_db = db.upcast();
        let name = trait_function_id.name(defs_db).into();
        let signature = db
            .trait_function_signature(trait_function_id)
            .map_err(|_| ABIError::CompilationError)?;

        let mut inputs = vec![];
        for param in signature.params.into_iter() {
            self.add_type(db, param.ty)?;
            inputs.push(Input { name: param.id.name(db.upcast()).into(), ty: param.ty.format(db) });
        }

        // TODO(spapini): output refs?
        let outputs = if signature.return_type.is_unit(db) {
            vec![]
        } else {
            self.add_type(db, signature.return_type)?;
            vec![Output { ty: signature.return_type.format(db) }]
        };

        self.abi.items.push(Item::Function(Function { name, inputs, outputs, state_mutability }));

        Ok(())
    }

    /// Adds an event to the ABI from a TraitFunctionId.
    fn add_event(
        &mut self,
        db: &dyn SemanticGroup,
        trait_function_id: TraitFunctionId,
    ) -> Result<(), ABIError> {
        let defs_db = db.upcast();
        let name = trait_function_id.name(defs_db).into();
        let signature = db
            .trait_function_signature(trait_function_id)
            .map_err(|_| ABIError::CompilationError)?;
        self.abi.items.push(Item::Event(Event {
            name,
            inputs: signature
                .params
                .into_iter()
                .map(|param| Input {
                    name: param.id.name(db.upcast()).into(),
                    ty: param.ty.format(db),
                })
                .collect(),
        }));

        Ok(())
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
        if is_native_type(db, &concrete) {
            return Ok(());
        }

        match concrete {
            ConcreteTypeId::Struct(id) => self.abi.items.push(Item::Struct(Struct {
                name: concrete.format(db),
                members: get_struct_members(db, id)?,
            })),
            ConcreteTypeId::Enum(id) => self.abi.items.push(Item::Enum(Enum {
                name: concrete.format(db),
                variants: get_enum_variants(db, id)?,
            })),
            ConcreteTypeId::Extern(_) => {}
        }
        Ok(())
    }
}

fn get_struct_members(
    db: &dyn SemanticGroup,
    id: cairo_lang_semantic::ConcreteStructId,
) -> Result<Vec<StructMember>, ABIError> {
    let mut res = vec![];
    for (name, member) in
        db.concrete_struct_members(id).map_err(|_| ABIError::UnexpectedType)?.iter()
    {
        res.push(StructMember { name: name.to_string(), ty: member.ty.format(db) })
    }
    Ok(res)
}

fn get_enum_variants(
    db: &dyn SemanticGroup,
    id: cairo_lang_semantic::ConcreteEnumId,
) -> Result<Vec<EnumVariant>, ABIError> {
    let generic_id = id.enum_id(db);
    let mut res = vec![];
    for (name, variant_id) in
        db.enum_variants(generic_id).map_err(|_| ABIError::UnexpectedType)?.iter()
    {
        res.push(EnumVariant {
            name: name.to_string(),
            ty: db
                .concrete_enum_variant(
                    id,
                    &db.variant_semantic(generic_id, *variant_id)
                        .map_err(|_| ABIError::UnexpectedType)?,
                )
                .map_err(|_| ABIError::UnexpectedType)?
                .ty
                .format(db),
        })
    }
    Ok(res)
}

/// Returns true if concrete is a native type.
///
/// native types are not added to the ABI.
fn is_native_type(db: &dyn SemanticGroup, concrete: &ConcreteTypeId) -> bool {
    let def_db = db.upcast();
    concrete.generic_type(db).parent_module(def_db).owning_crate(def_db) == db.core_crate()
}

/// Checks whether the trait function has the given attribute.
fn trait_function_has_attr(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
    attr: &str,
) -> Result<bool, ABIError> {
    Ok(db
        .trait_function_attributes(trait_function_id)
        .map_err(|_| ABIError::CompilationError)?
        .iter()
        .any(|a| a.id.to_string() == attr))
}

#[derive(Error, Debug)]
pub enum ABIError {
    #[error("Generic traits are unsupported.")]
    GenericTraitsUnsupported,
    #[error("Compilation error.")]
    CompilationError,
    #[error("Got unexpected type.")]
    UnexpectedType,
}

/// Enum of contract item ABIs.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Item {
    #[serde(rename = "function")]
    Function(Function),
    #[serde(rename = "event")]
    Event(Event),
    #[serde(rename = "struct")]
    Struct(Struct),
    #[serde(rename = "enum")]
    Enum(Enum),
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum StateMutability {
    #[serde(rename = "external")]
    External,
    #[serde(rename = "view")]
    View,
}

/// Contract function ABI.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub inputs: Vec<Input>,

    // TODO(ilya): Should the output be a vector or a single type?
    pub outputs: Vec<Output>,
    pub state_mutability: StateMutability,
}

/// Contract event.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Event {
    pub name: String,
    pub inputs: Vec<Input>,
}

/// Function input ABI.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Input {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
}

/// Function Output ABI.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Output {
    #[serde(rename = "type")]
    pub ty: String,
}

/// Struct ABI.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Struct {
    pub name: String,
    pub members: Vec<StructMember>,
}

/// Struct member.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructMember {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
}

/// Enum ABI.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

/// Enum variant.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnumVariant {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
}
