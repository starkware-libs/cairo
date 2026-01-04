//! Types for the type hash derive macro as defined in the SNIP-12.
//!
//! There are 4 kinds of types:
//!
//! 1. Basic types: defined in the spec for a given revision. Ex: felt, ClassHash, timestamp, u128...
//! 4. Collection types: they are arrays or tuples of other types.
//! 2. Preset types: they are structs defined in the spec. Ex: TokenAmount, NftId, u256. They also depend on the revision used.
//! 3. User defined types: The ones in the "types" field of the request. They also include the domain separator (Ex. StarknetDomain)

use cairo_lang_macro::Diagnostic;

use super::diagnostics::errors;

/// The different types of types as defined in the SNIP-12.
#[derive(Debug)]
pub enum S12Type {
    Basic(BasicType),
    Collection(CollectionType),
    Preset(PresetType),
    UserDefined(UserDefinedType),
}

/// The different basic types as defined in the SNIP-12.
#[derive(Debug)]
pub enum BasicType {
    Felt,
    ShortString,
    ClassHash,
    ContractAddress,
    Timestamp,
    Selector,
    MerkleTree,
    U128,
    I128,
}

/// The different types of collections supported by the SNIP-12.
#[derive(Debug)]
pub enum CollectionType {
    Tuple(Vec<S12Type>),
    Array(Box<S12Type>),
}

/// The different preset types as defined in the SNIP-12.
#[derive(Debug)]
pub enum PresetType {
    TokenAmount,
    NftId,
    U256,
}

/// The different user defined types as defined in the SNIP-12.
///
/// They include the domain separator.
#[derive(Debug)]
pub enum UserDefinedType {
    StarknetDomain,
    Custom(String),
}

/// A type that is referenced as a member of the input type.
pub struct InnerType {
    pub name: String,
    pub encoded_type: String,
}

impl S12Type {
    /// Creates a S12Type from a String
    pub fn from_str(s: &str) -> Option<S12Type> {
        let s = s.trim();

        // Check if the type is a tuple
        if s.starts_with("(") && s.ends_with(")") {
            let types = split_types(&s[1..s.len() - 1])
                .iter()
                .map(|s| S12Type::from_str(s).unwrap())
                .collect();
            return Some(S12Type::Collection(CollectionType::Tuple(types)));
        }

        // Check if the type is an array/span
        if s.starts_with("Array<") || s.starts_with("Span<") {
            let prefix_len = if s.starts_with("Array<") { 6 } else { 5 };
            let array_type = Box::new(S12Type::from_str(&s[prefix_len..s.len() - 1]).unwrap());
            return Some(S12Type::Collection(CollectionType::Array(array_type)));
        }

        Some(match s {
            // Check empty string
            "" => return None,

            // Check basic types
            "felt252" => S12Type::Basic(BasicType::Felt),
            "shortstring" => S12Type::Basic(BasicType::ShortString),
            "ClassHash" => S12Type::Basic(BasicType::ClassHash),
            "ContractAddress" => S12Type::Basic(BasicType::ContractAddress),
            "timestamp" => S12Type::Basic(BasicType::Timestamp),
            "selector" => S12Type::Basic(BasicType::Selector),
            "merkletree" => S12Type::Basic(BasicType::MerkleTree),
            "u128" => S12Type::Basic(BasicType::U128),
            "i128" => S12Type::Basic(BasicType::I128),

            // Check preset types
            "TokenAmount" => S12Type::Preset(PresetType::TokenAmount),
            "NftId" => S12Type::Preset(PresetType::NftId),
            "u256" => S12Type::Preset(PresetType::U256),

            // Check user defined types
            "StarknetDomain" => S12Type::UserDefined(UserDefinedType::StarknetDomain),

            // Custom type
            _ => S12Type::UserDefined(UserDefinedType::Custom(s.to_string())),
        })
    }

    /// Returns the SNIP-12 type name for the S12Type.
    ///
    /// Example:
    /// ```
    /// let type_hash = S12Type::from_str("felt252").unwrap();
    /// assert_eq!(type_hash.get_snip12_type_name().unwrap(), "felt");
    /// ```
    pub fn get_snip12_type_name(&self) -> Result<String, Diagnostic> {
        match self {
            S12Type::Basic(basic_type) => basic_type.get_snip12_type_name(),
            S12Type::Collection(collection_type) => collection_type.get_snip12_type_name(),
            S12Type::Preset(preset_type) => preset_type.get_snip12_type_name(),
            S12Type::UserDefined(user_defined_type) => user_defined_type.get_snip12_type_name(),
        }
    }

    /// Returns the encoded type for the S12Type.
    ///
    /// If the type is not an object/enum meaning it's a basic type, it returns an empty string.
    /// NOTE: It doesn't append the inner types to the encoded type. This should be done by the caller.
    ///
    /// Example:
    /// ```
    /// let type_hash = S12Type::from_str("u256").unwrap();
    /// let (encoded_type, inner_types) = type_hash.get_encoded_ref_type().unwrap();
    /// assert_eq!(encoded_type, "\"u256\"(\"low\":\"u128\",\"high\":\"u128\")");
    /// assert_eq!(inner_types, vec![]);
    /// ```
    pub fn get_encoded_ref_type(&self) -> Result<(String, Vec<InnerType>), Diagnostic> {
        match self {
            S12Type::Basic(basic_type) => basic_type.get_encoded_ref_type(),
            S12Type::Collection(collection_type) => collection_type.get_encoded_ref_type(),
            S12Type::Preset(preset_type) => preset_type.get_encoded_ref_type(),
            S12Type::UserDefined(user_defined_type) => user_defined_type.get_encoded_ref_type(),
        }
    }
}

impl BasicType {
    /// Returns the SNIP-12 type name for the BasicType.
    pub fn get_snip12_type_name(&self) -> Result<String, Diagnostic> {
        Ok(match self {
            BasicType::Felt => "felt",
            BasicType::ShortString => "shortstring",
            BasicType::ClassHash => "ClassHash",
            BasicType::ContractAddress => "ContractAddress",
            BasicType::Timestamp => "timestamp",
            BasicType::Selector => "selector",
            BasicType::MerkleTree => "merkletree",
            BasicType::U128 => "u128",
            BasicType::I128 => "i128",
        }
        .to_string())
    }

    /// Returns the encoded type for the BasicType.
    ///
    /// NOTE: since basic types are not objects/enums, they don't need a referenced encoded type.
    pub fn get_encoded_ref_type(&self) -> Result<(String, Vec<InnerType>), Diagnostic> {
        match self {
            BasicType::Felt
            | BasicType::ShortString
            | BasicType::ClassHash
            | BasicType::ContractAddress
            | BasicType::Timestamp
            | BasicType::Selector
            | BasicType::U128
            | BasicType::I128
            | BasicType::MerkleTree => Ok((String::new(), vec![])),
        }
    }
}

impl CollectionType {
    /// Returns the SNIP-12 type name for the CollectionType.
    pub fn get_snip12_type_name(&self) -> Result<String, Diagnostic> {
        Ok(match self {
            CollectionType::Tuple(types) => {
                let mut types_str = Vec::new();
                for t in types.iter() {
                    types_str.push(t.get_snip12_type_name()?);
                }
                format!("({})", types_str.join(","))
            }
            CollectionType::Array(array_type) => format!("{}*", array_type.get_snip12_type_name()?),
        }
        .to_string())
    }

    /// Returns the encoded type for the CollectionType.
    ///
    /// NOTE: Collection types are not objects/enums, they don't need a referenced encoded type,
    /// but they may have inner types that need to be referenced.
    pub fn get_encoded_ref_type(&self) -> Result<(String, Vec<InnerType>), Diagnostic> {
        Ok(match self {
            CollectionType::Tuple(types) => {
                let mut inner_types = vec![];
                for t in types.iter() {
                    let (encoded_type, inner_types_rec) = t.get_encoded_ref_type()?;
                    // If the encoded type is not empty, it means the type needs to be referenced
                    if !encoded_type.is_empty() {
                        inner_types.push(InnerType {
                            name: t.get_snip12_type_name()?,
                            encoded_type,
                        });
                    }

                    // Add the inner types of the tuple recursively
                    inner_types.extend(inner_types_rec);
                }
                (String::new(), inner_types)
            }
            CollectionType::Array(array_type) => {
                let (encoded_type, mut inner_types) = array_type.get_encoded_ref_type()?;
                // If the encoded type is not empty, it means the array type needs to be referenced
                if !encoded_type.is_empty() {
                    inner_types.push(InnerType {
                        name: array_type.get_snip12_type_name()?,
                        encoded_type,
                    });
                }
                (String::new(), inner_types)
            }
        })
    }
}

impl PresetType {
    /// Returns the SNIP-12 type name for the PresetType.
    pub fn get_snip12_type_name(&self) -> Result<String, Diagnostic> {
        Ok(match self {
            PresetType::TokenAmount => "TokenAmount",
            PresetType::NftId => "NftId",
            PresetType::U256 => "u256",
        }
        .to_string())
    }

    /// Returns the encoded type for the PresetType.
    pub fn get_encoded_ref_type(&self) -> Result<(String, Vec<InnerType>), Diagnostic> {
        // u256 inner type representation
        let u256_encoded_type = "\"u256\"(\"low\":\"u128\",\"high\":\"u128\")".to_string();
        let u256_inner_type = InnerType {
            name: "u256".to_string(),
            encoded_type: u256_encoded_type.clone(),
        };

        Ok(match self {
            PresetType::TokenAmount => (
                "\"TokenAmount\"(\
                \"token_address\":\"ContractAddress\",\
                \"amount\":\"u256\")"
                    .to_string(),
                vec![u256_inner_type],
            ),
            PresetType::NftId => (
                "\"NftId\"(\
                \"collection_address\":\"ContractAddress\",\
                \"token_id\":\"u256\")"
                    .to_string(),
                vec![u256_inner_type],
            ),
            PresetType::U256 => (u256_encoded_type, vec![]),
        })
    }
}

impl UserDefinedType {
    /// Returns the SNIP-12 type name for the UserDefinedType.
    pub fn get_snip12_type_name(&self) -> Result<String, Diagnostic> {
        Ok(match self {
            UserDefinedType::StarknetDomain => "StarknetDomain",
            UserDefinedType::Custom(name) => name,
        }
        .to_string())
    }

    /// Returns the encoded type for the UserDefinedType.
    pub fn get_encoded_ref_type(&self) -> Result<(String, Vec<InnerType>), Diagnostic> {
        match self {
            UserDefinedType::StarknetDomain => Ok((
                "\"StarknetDomain\"(\
                \"name\":\"shortstring\",\
                \"version\":\"shortstring\",\
                \"chainId\":\"shortstring\",\
                \"revision\":\"shortstring\")"
                    .to_string(),
                vec![],
            )),
            UserDefinedType::Custom(_) => Err(Diagnostic::error(errors::CUSTOM_TYPE_NOT_SUPPORTED)),
        }
    }
}

/// Returns a list of comma separated strings, ignoring commas inside matching parentheses.
///
/// Example:
/// ```
/// let s = "a,b,(c,d),e";
/// let result = split_types(s);
/// assert_eq!(result, vec!["a", "b", "(c,d)", "e"]);
/// ```
pub fn split_types(s: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut start = 0;
    let mut paren_count = 0;

    for (i, c) in s.chars().enumerate() {
        match c {
            '(' => paren_count += 1,
            ')' => paren_count -= 1,
            ',' if paren_count == 0 => {
                if start < i {
                    result.push(s[start..i].trim());
                }
                start = i + 1;
            }
            _ => {}
        }
    }

    // Add the last segment
    if start < s.len() {
        result.push(s[start..].trim());
    }

    result
}
