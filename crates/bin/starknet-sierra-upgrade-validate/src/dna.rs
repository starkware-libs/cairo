//! Minimal hand-rolled definitions of the Apibara DNA v2 gRPC protocol, covering only the parts
//! required for streaming the declared classes of a block range.
//!
//! Prost skips unknown fields and `oneof` variants while decoding, so only the fields actually
//! read are declared. See <https://github.com/apibara/dna> for the full protocol definitions.

#[cfg(test)]
#[path = "dna_test.rs"]
mod test;

use num_bigint::BigUint;
use prost::Message;

/// The gRPC method for streaming data from a DNA server.
pub const STREAM_DATA_PATH: &str = "/dna.v2.stream.DnaStream/StreamData";

/// `dna.v2.stream.DataFinality.DATA_FINALITY_ACCEPTED` - blocks that are part of the canonical
/// chain.
pub const DATA_FINALITY_ACCEPTED: i32 = 2;

/// `starknet.v2.HeaderFilter.HEADER_FILTER_ALWAYS` - receive a message for every block, even if
/// no other filter matches.
pub const HEADER_FILTER_ALWAYS: i32 = 1;

/// `dna.v2.stream.Cursor` - a position in the stream.
#[derive(Clone, PartialEq, Message)]
pub struct Cursor {
    /// The block number.
    #[prost(uint64, tag = "1")]
    pub order_key: u64,
    /// The block hash. May be empty when only the block number is known.
    #[prost(bytes = "vec", tag = "2")]
    pub unique_key: Vec<u8>,
}

/// `dna.v2.stream.StreamDataRequest` - a request to stream data from the server.
#[derive(Clone, PartialEq, Message)]
pub struct StreamDataRequest {
    /// The cursor to start streaming right after. Streams from genesis if unset.
    #[prost(message, optional, tag = "1")]
    pub starting_cursor: Option<Cursor>,
    /// The requested `DataFinality` of the returned data.
    #[prost(int32, optional, tag = "2")]
    pub finality: Option<i32>,
    /// The protobuf-encoded chain-specific filters.
    #[prost(bytes = "vec", repeated, tag = "3")]
    pub filter: Vec<Vec<u8>>,
}

/// `dna.v2.stream.StreamDataResponse` - a message from the server.
#[derive(Clone, PartialEq, Message)]
pub struct StreamDataResponse {
    /// Other variants (invalidate, finalize, heartbeat, system message) decode to `None`.
    #[prost(oneof = "stream_data_response::Message", tags = "1")]
    pub message: Option<stream_data_response::Message>,
}
pub mod stream_data_response {
    /// The relevant variants of the `dna.v2.stream.StreamDataResponse.message` oneof.
    #[derive(Clone, PartialEq, prost::Oneof)]
    pub enum Message {
        /// The data of a single block.
        #[prost(message, tag = "1")]
        Data(super::Data),
    }
}

/// `dna.v2.stream.Data` - the data of a single block.
#[derive(Clone, PartialEq, Message)]
pub struct Data {
    /// The cursor of the block the data belongs to.
    #[prost(message, optional, tag = "2")]
    pub end_cursor: Option<Cursor>,
    /// A protobuf-encoded `Block` per requested filter.
    #[prost(bytes = "vec", repeated, tag = "4")]
    pub data: Vec<Vec<u8>>,
}

/// `starknet.v2.Filter` - a filter over the data of a single Starknet block.
#[derive(Clone, PartialEq, Message)]
pub struct Filter {
    /// A `HeaderFilter` value, controlling when the block header is returned.
    #[prost(int32, tag = "1")]
    pub header: i32,
    /// The requested contract changes.
    #[prost(message, repeated, tag = "6")]
    pub contract_changes: Vec<ContractChangeFilter>,
}

/// `starknet.v2.ContractChangeFilter` - a filter over class/contract changes.
#[derive(Clone, PartialEq, Message)]
pub struct ContractChangeFilter {
    #[prost(oneof = "contract_change_filter::Change", tags = "2")]
    pub change: Option<contract_change_filter::Change>,
}
pub mod contract_change_filter {
    /// The relevant variants of the `starknet.v2.ContractChangeFilter.change` oneof.
    #[expect(clippy::enum_variant_names, reason = "Named after the protocol definition.")]
    #[derive(Clone, PartialEq, prost::Oneof)]
    pub enum Change {
        /// Request declared classes.
        #[prost(message, tag = "2")]
        DeclaredClass(super::DeclaredClassFilter),
    }
}

/// `starknet.v2.DeclaredClassFilter` - matches all declared classes.
#[derive(Clone, PartialEq, Message)]
pub struct DeclaredClassFilter {}

/// `starknet.v2.Block` - the filtered data of a single Starknet block.
#[derive(Clone, PartialEq, Message)]
pub struct Block {
    /// The class/contract changes matching the filter.
    #[prost(message, repeated, tag = "7")]
    pub contract_changes: Vec<ContractChange>,
}

/// `starknet.v2.ContractChange` - a single class/contract change.
#[derive(Clone, PartialEq, Message)]
pub struct ContractChange {
    /// Other variants (replaced class, deployed contract) decode to `None`.
    #[prost(oneof = "contract_change::Change", tags = "2")]
    pub change: Option<contract_change::Change>,
}
pub mod contract_change {
    /// The relevant variants of the `starknet.v2.ContractChange.change` oneof.
    #[expect(clippy::enum_variant_names, reason = "Named after the protocol definition.")]
    #[derive(Clone, PartialEq, prost::Oneof)]
    pub enum Change {
        /// A class declaration.
        #[prost(message, tag = "2")]
        DeclaredClass(super::DeclaredClass),
    }
}

/// `starknet.v2.DeclaredClass` - a single class declaration.
#[derive(Clone, PartialEq, Message)]
pub struct DeclaredClass {
    /// The hash of the declared class.
    #[prost(message, optional, tag = "1")]
    pub class_hash: Option<FieldElement>,
    /// The hash of the casm resulting from the Sierra compilation.
    ///
    /// Unset for deprecated Cairo 0 declarations.
    #[prost(message, optional, tag = "2")]
    pub compiled_class_hash: Option<FieldElement>,
}

/// `starknet.v2.FieldElement` - a field element as 4 big-endian limbs.
#[derive(Clone, Copy, PartialEq, Message)]
pub struct FieldElement {
    #[prost(fixed64, tag = "1")]
    pub x0: u64,
    #[prost(fixed64, tag = "2")]
    pub x1: u64,
    #[prost(fixed64, tag = "3")]
    pub x2: u64,
    #[prost(fixed64, tag = "4")]
    pub x3: u64,
}
impl FieldElement {
    /// Returns the value as a `BigUint`.
    pub fn to_biguint(self) -> BigUint {
        let mut bytes = [0u8; 32];
        for (chunk, limb) in
            bytes.as_chunks_mut::<8>().0.iter_mut().zip([self.x0, self.x1, self.x2, self.x3])
        {
            *chunk = limb.to_be_bytes();
        }
        BigUint::from_bytes_be(&bytes)
    }
}
