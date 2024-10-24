use core::ecdsa::check_ecdsa_signature;

const message_hash: felt252 = 0x2d6479c0758efbb5aa07d35ed5454d728637fceab7ba544d3ea95403a5630a8;

const pubkey: felt252 = 0x1ef15c18599971b7beced415a40f0c7deacfd9b0d1819e03d723d8bc943cfca;
const r: felt252 = 0x6ff7b413a8457ef90f326b5280600a4473fef49b5b1dcdfcd7f42ca7aa59c69;
const s: felt252 = 0x23a9747ed71abc5cb956c0df44ee8638b65b3e9407deade65de62247b8fd77;


#[test]
fn test_check_ecdsa_signature() {
    check_ecdsa_signature(message_hash, pubkey, r, s);
}
