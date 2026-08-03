//! Tests of the hand-rolled DNA protocol definitions against golden wire encodings.
//!
//! The golden bytes were verified with `protoc` against the official protocol definitions from
//! the `apibara-dna-protocol` crate (v2.0.0).

use num_bigint::BigUint;
use prost::Message;

use super::{
    Block, ContractChange, ContractChangeFilter, Cursor, DATA_FINALITY_ACCEPTED, Data,
    DeclaredClass, DeclaredClassFilter, FieldElement, Filter, HEADER_FILTER_ALWAYS,
    StreamDataRequest, StreamDataResponse, contract_change, contract_change_filter,
    stream_data_response,
};

/// Decodes a hex string into bytes.
fn from_hex(hex: &str) -> Vec<u8> {
    (0..hex.len()).step_by(2).map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap()).collect()
}

#[test]
fn stream_data_request_encoding() {
    let filter = Filter {
        header: HEADER_FILTER_ALWAYS,
        contract_changes: vec![ContractChangeFilter {
            change: Some(contract_change_filter::Change::DeclaredClass(DeclaredClassFilter {})),
        }],
    };
    let request = StreamDataRequest {
        starting_cursor: Some(Cursor { order_key: 699999, unique_key: vec![] }),
        finality: Some(DATA_FINALITY_ACCEPTED),
        filter: vec![filter.encode_to_vec()],
    };
    assert_eq!(request.encode_to_vec(), from_hex("0a0408dfdc2a10021a06080132021200"));
}

#[test]
fn stream_data_response_decoding() {
    let response = StreamDataResponse::decode(
        from_hex(
            "0a5c120808e0dc2a1202abcd22503a4e124c0a2409010000000000000011020000000000000019030000\
             000000000021040000000000000012240905000000000000001106000000000000001907000000000000\
             00210800000000000000",
        )
        .as_slice(),
    )
    .unwrap();
    let Some(stream_data_response::Message::Data(Data { end_cursor, data })) = response.message
    else {
        panic!("Expected a data message.");
    };
    assert_eq!(end_cursor, Some(Cursor { order_key: 700000, unique_key: vec![0xab, 0xcd] }));
    let [block_data] = data.as_slice() else {
        panic!("Expected a single block.");
    };
    let block = Block::decode(block_data.as_slice()).unwrap();
    let Block { contract_changes } = block;
    let class_hash = FieldElement { x0: 1, x1: 2, x2: 3, x3: 4 };
    let compiled_class_hash = FieldElement { x0: 5, x1: 6, x2: 7, x3: 8 };
    assert_eq!(
        contract_changes,
        vec![ContractChange {
            change: Some(contract_change::Change::DeclaredClass(DeclaredClass {
                class_hash: Some(class_hash),
                compiled_class_hash: Some(compiled_class_hash),
            })),
        }]
    );
    assert_eq!(
        class_hash.to_biguint(),
        BigUint::from(1u8) << 192
            | BigUint::from(2u8) << 128
            | BigUint::from(3u8) << 64
            | BigUint::from(4u8)
    );
}

#[test]
fn non_data_response_decoding() {
    // A heartbeat message - field 4 of the `message` oneof, which is not declared and expected
    // to be skipped.
    let response = StreamDataResponse::decode(from_hex("2200").as_slice()).unwrap();
    assert_eq!(response.message, None);
}
