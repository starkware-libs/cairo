pub extern crate alloc;

pub use alloc::{boxed, format, rc, string, sync, vec};
// Allow interpreting vectors of bytes as strings, but not constructing them.
pub use core::str;
pub use core::{
    any, cell, clone, cmp, convert, default, fmt, hash, iter, marker, mem, num, ops, ptr, result,
    slice, stringify, time,
};

pub mod collections {
    pub use alloc::collections::{btree_map, btree_set, vec_deque};

    pub use hashbrown::{hash_map, hash_set, HashMap, HashSet};
}

pub mod borrow {
    pub use alloc::borrow::*;
    pub use core::borrow::*;
}
