pub use std::{
    alloc, any, borrow, boxed, cell, clone, cmp, convert, default, fmt, format, hash, iter, marker,
    mem, num, ops, ptr, rc, result, slice, str, string, stringify, sync, vec,
};

pub mod collections {
    pub use std::collections::{btree_map, btree_set, hash_set, vec_deque, HashMap, HashSet};

    pub mod hash_map {
        pub use std::collections::hash_map::*;

        pub type DefaultHashBuilder =
            std::hash::BuildHasherDefault<std::collections::hash_map::DefaultHasher>;
    }
}
