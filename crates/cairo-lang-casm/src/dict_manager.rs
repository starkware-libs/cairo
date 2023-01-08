use std::collections::HashMap;

use cairo_vm::types::relocatable::Relocatable;
use cairo_vm::vm::vm_core::VirtualMachine;
use num_bigint::BigInt;

/// Stores the data of a specific dictionary.
pub struct DictTrackerExecScope {
    /// The data of the dictionary.
    data: HashMap<BigInt, BigInt>,
    /// The index of the dictionary in the dict_infos segement.
    idx: usize,
}

/// Helper object to allocate, track and destruct all dictionaries in the run.
#[derive(Default)]
pub struct DictManagerExecScope {
    /// Maps between a segment index and the DictTrackerExecScope associated with it.
    trackers: HashMap<isize, DictTrackerExecScope>,
}

impl DictTrackerExecScope {
    /// Creates a new tracker placed in index `idx` in the dict_infos segment.
    pub fn new(idx: usize) -> Self {
        Self { data: HashMap::default(), idx }
    }
}

impl DictManagerExecScope {
    pub const DICT_DEFAULT_VALUE: usize = 0;

    /// Allocates a new segment for a new dictionary and return the start of the segment.
    pub fn new_default_dict(&mut self, vm: &mut VirtualMachine) -> Relocatable {
        let dict_segment = vm.add_memory_segment();
        assert!(
            self.trackers
                .insert(dict_segment.segment_index, DictTrackerExecScope::new(self.trackers.len()))
                .is_none(),
            "Segment index already in use."
        );
        dict_segment
    }

    /// Returns a reference for a dict tracker corresponding to a given pointer to a dict segment.
    fn get_dict_tracker(&self, dict_end: Relocatable) -> &DictTrackerExecScope {
        self.trackers
            .get(&dict_end.segment_index)
            .expect("The given value does not point to a known dictionary.")
    }

    /// Returns a mut reference for a dict tracker corresponding to a given pointer to a dict
    /// segment.
    fn get_dict_tracker_mut(&mut self, dict_end: Relocatable) -> &mut DictTrackerExecScope {
        self.trackers
            .get_mut(&dict_end.segment_index)
            .expect("The given value does not point to a known dictionary.")
    }

    /// Returns the index of the dict tracker corresponding to a given pointer to a dict segment.
    pub fn get_dict_infos_index(&self, dict_end: Relocatable) -> usize {
        self.get_dict_tracker(dict_end).idx
    }

    /// Inserts a value to the dict tracker corresponding to a given pointer to a dict segment.
    pub fn insert_to_tracker(&mut self, dict_end: Relocatable, key: BigInt, value: BigInt) {
        self.get_dict_tracker_mut(dict_end).data.insert(key, value);
    }

    /// Gets a value from the dict tracker corresponding to a given pointer to a dict segment.
    /// None if the key does not exist in the tracker data.
    pub fn get_from_tracker(&self, dict_end: Relocatable, key: &BigInt) -> Option<BigInt> {
        self.get_dict_tracker(dict_end).data.get(key).cloned()
    }
}
