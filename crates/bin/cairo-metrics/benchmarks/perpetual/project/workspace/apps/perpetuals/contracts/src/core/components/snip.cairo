use openzeppelin::utils::snip12::SNIP12Metadata;
use crate::core::constants::{NAME, VERSION};

#[embeddable_as(SnipImpl)]
pub impl SNIP12MetadataImpl of SNIP12Metadata {
    fn name() -> felt252 {
        NAME
    }
    fn version() -> felt252 {
        VERSION
    }
}
