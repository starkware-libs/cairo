use std::collections::HashMap;

use cairo_felt::Felt252;
use cairo_vm::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_vm::vm::vm_core::VirtualMachine;

/// Stores the data of a specific dictionary.
pub struct DictTrackerExecScope {
    /// The data of the dictionary.
    data: HashMap<Felt252, MaybeRelocatable>,
    /// The index of the dictionary in the dict_infos segment.
    #[allow(dead_code)]
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
    pub fn insert_to_tracker(
        &mut self,
        dict_end: Relocatable,
        key: Felt252,
        value: MaybeRelocatable,
    ) {
        self.get_dict_tracker_mut(dict_end).data.insert(key, value);
    }

    /// Gets a value from the dict tracker corresponding to a given pointer to a dict segment.
    /// None if the key does not exist in the tracker data.
    pub fn get_from_tracker(
        &self,
        dict_end: Relocatable,
        key: &Felt252,
    ) -> Option<MaybeRelocatable> {
        self.get_dict_tracker(dict_end).data.get(key).cloned()
    }
}

/// Helper object for the management of dict_squash hints.
#[derive(Default, Debug)]
pub struct DictSquashExecScope {
    /// A map from key to the list of indices accessing it, each list in reverse order.
    pub access_indices: HashMap<Felt252, Vec<Felt252>>,
    /// Descending list of keys.
    pub keys: Vec<Felt252>,
}

impl DictSquashExecScope {
    /// Returns the current key to process.
    pub fn current_key(&self) -> Option<Felt252> {
        self.keys.last().cloned()
    }

    /// Returns and removes the current key, and its access indices. Should be called when only the
    /// last key access is in the corresponding indices list.
    pub fn pop_current_key(&mut self) -> Option<Felt252> {
        let key_accesses = self.access_indices.remove(&self.current_key().unwrap());
        assert!(
            key_accesses.unwrap().len() == 1,
            "Key popped but not all accesses were processed."
        );
        self.keys.pop()
    }

    /// Returns a reference to the access indices list of the current key.
    pub fn current_access_indices(&mut self) -> Option<&mut Vec<Felt252>> {
        let current_key = self.current_key()?;
        self.access_indices.get_mut(&current_key)
    }

    /// Returns a reference to the last index in the current access indices list.
    pub fn current_access_index(&mut self) -> Option<&Felt252> {
        self.current_access_indices()?.last()
    }

    /// Returns and removes the current access index.
    pub fn pop_current_access_index(&mut self) -> Option<Felt252> {
        self.current_access_indices()?.pop()
    }
}
