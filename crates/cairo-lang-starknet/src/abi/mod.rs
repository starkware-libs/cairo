use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

#[cfg(feature = "std")]
mod builder;
#[cfg(feature = "std")]
pub use builder::*;

#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};

/// Contract ABI.
#[derive(Clone, Default, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(transparent))]
pub struct Contract {
    // TODO(spapini): Add storage variables.
    items: OrderedHashSet<Item>,
}
impl Contract {
    #[cfg(feature = "serde")]
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
}

impl IntoIterator for Contract {
    type Item = Item;
    type IntoIter = <OrderedHashSet<Item> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}
/// Enum of contract item ABIs.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(tag = "type"))]
pub enum Item {
    #[cfg_attr(feature = "serde", serde(rename = "function"))]
    Function(Function),
    #[cfg_attr(feature = "serde", serde(rename = "constructor"))]
    Constructor(Constructor),
    #[cfg_attr(feature = "serde", serde(rename = "l1_handler"))]
    L1Handler(L1Handler),
    #[cfg_attr(feature = "serde", serde(rename = "event"))]
    Event(Event),
    #[cfg_attr(feature = "serde", serde(rename = "struct"))]
    Struct(Struct),
    #[cfg_attr(feature = "serde", serde(rename = "enum"))]
    Enum(Enum),
    #[cfg_attr(feature = "serde", serde(rename = "interface"))]
    Interface(Interface),
    #[cfg_attr(feature = "serde", serde(rename = "impl"))]
    Impl(Imp),
}

/// Contract interface ABI.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Interface {
    pub name: String,
    pub items: Vec<Item>,
}

/// Contract impl ABI.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Imp {
    pub name: String,
    pub interface_name: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum StateMutability {
    #[cfg_attr(feature = "serde", serde(rename = "external"))]
    External,
    #[cfg_attr(feature = "serde", serde(rename = "view"))]
    View,
}

/// Contract function ABI.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Function {
    pub name: String,
    pub inputs: Vec<Input>,

    // TODO(ilya): Should the output be a vector or a single type?
    pub outputs: Vec<Output>,
    pub state_mutability: StateMutability,
}

/// Contract constructor ABI.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Constructor {
    pub name: String,
    pub inputs: Vec<Input>,
}

/// Contract L1 handler ABI.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct L1Handler {
    pub name: String,
    pub inputs: Vec<Input>,

    // TODO(ilya): Should the output be a vector or a single type?
    pub outputs: Vec<Output>,
    pub state_mutability: StateMutability,
}

/// Contract event.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Event {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(flatten))]
    pub kind: EventKind,
}

/// Contract event kind.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize), serde(tag = "kind"))]
pub enum EventKind {
    #[cfg_attr(feature = "serde", serde(rename = "struct"))]
    Struct { members: Vec<EventField> },
    #[cfg_attr(feature = "serde", serde(rename = "enum"))]
    Enum { variants: Vec<EventField> },
}

/// Contract event field (member/variant).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct EventField {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    pub ty: String,
    pub kind: EventFieldKind,
}

/// Describes how to serialize the event's field.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum EventFieldKind {
    // Serialize to `keys` using `Serde`.
    #[cfg_attr(feature = "serde", serde(rename = "key"))]
    KeySerde,
    // Serialize to `data` using `Serde`.
    #[cfg_attr(feature = "serde", serde(rename = "data"))]
    DataSerde,
    // Serialize as a nested event.
    #[cfg_attr(feature = "serde", serde(rename = "nested"))]
    Nested,
    // Serialize as a flat event.
    #[cfg_attr(feature = "serde", serde(rename = "flat"))]
    Flat,
}

/// Function input ABI.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Input {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    pub ty: String,
}

/// Function Output ABI.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Output {
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    pub ty: String,
}

/// Struct ABI.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Struct {
    pub name: String,
    pub members: Vec<StructMember>,
}

/// Struct member.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StructMember {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    pub ty: String,
}

/// Enum ABI.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Enum {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

/// Enum variant.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct EnumVariant {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(rename = "type"))]
    pub ty: String,
}
