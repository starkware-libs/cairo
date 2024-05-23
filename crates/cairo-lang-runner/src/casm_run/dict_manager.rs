use std::collections::HashMap;

use cairo_felt::Felt252;
use cairo_vm::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_vm::vm::errors::hint_errors::HintError;
use cairo_vm::vm::vm_core::VirtualMachine;

use num_traits::One;

/// Stores the data of a specific dictionary.
pub struct DictTrackerExecScope {
    /// The data of the dictionary.
    data: HashMap<Felt252, MaybeRelocatable>,
    /// The start of the segment of the dictionary.
    start: Relocatable,
    /// The start of the next segment in the segment arena, if finalized.
    next_start: Option<Relocatable>,
}

/// Helper object to allocate, track and destruct all dictionaries in the run.
#[derive(Default)]
pub struct DictManagerExecScope {
    /// Maps between a segment index and the DictTrackerExecScope associated with it.
    segment_to_tracker: HashMap<isize, usize>,
    /// The actual trackers of the dictionaries, in the order of allocation.
    trackers: Vec<DictTrackerExecScope>,
}

impl DictTrackerExecScope {
    /// Creates a new tracker starting at `start`.
    pub fn new(start: Relocatable) -> Self {
        Self { data: HashMap::default(), start, next_start: None }
    }
}

impl DictManagerExecScope {
    pub const DICT_DEFAULT_VALUE: usize = 0;

    /// Allocates a new segment for a new dictionary and return the start of the segment.
    pub fn new_default_dict(&mut self, vm: &mut VirtualMachine) -> Result<Relocatable, HintError> {
        let dict_segment = match self.trackers.last() {
            // This is the first dict - a totally new segment is required.
            None => vm.add_memory_segment(),
            // New dict segment should be appended to the last segment.
            // Appending by a temporary segment, if the last segment is not finalized.
            Some(last) => last.next_start.unwrap_or_else(|| vm.add_temporary_segment()),
        };
        let tracker = DictTrackerExecScope::new(dict_segment);
        // Not checking if overriding - since overriding is allowed.
        self.segment_to_tracker.insert(dict_segment.segment_index, self.trackers.len());

        self.trackers.push(tracker);
        Ok(dict_segment)
    }

    /// Returns a mut reference for a dict tracker corresponding to a given pointer to a dict
    /// segment.
    fn get_dict_tracker_mut(&mut self, dict_end: Relocatable) -> &mut DictTrackerExecScope {
        let idx = self
            .get_dict_infos_index(dict_end)
            .expect("The given value does not point to a known dictionary.");
        &mut self.trackers[idx]
    }

    /// Returns the index of the dict tracker corresponding to a given pointer to a dict segment.
    pub fn get_dict_infos_index(&self, dict_end: Relocatable) -> Result<usize, HintError> {
        Ok(*self.segment_to_tracker.get(&dict_end.segment_index).ok_or_else(|| {
            HintError::CustomHint(
                "The given value does not point to a known dictionary."
                    .to_string()
                    .into_boxed_str(),
            )
        })?)
    }

    /// Finalizes a segment of a dictionary and attempts to relocate it.
    /// Relocates the dictionary if the previous dictionary was also relocated, if not, assigns a temporary next_start to aid in a future relocation.
    /// Relocates the next dictionary if it was already finalized but not relocated.
    pub fn finalize_segment(
        &mut self,
        vm: &mut VirtualMachine,
        dict_end: Relocatable,
    ) -> Result<(), HintError> {
        let tracker_idx = self.get_dict_infos_index(dict_end).unwrap();
        if self.trackers[tracker_idx].start.segment_index >= 0 {
            // The dict is already on a real segment so we don't need to relocate it
            // This is the case of the first dictionary
            self.trackers[tracker_idx].next_start = Some(dict_end);
        } else {
            // Finalize & relocate the dictionary
            // The first dictionary will always be on a real segment so we can be sure that a previous dictionary exists here
            match self.trackers[tracker_idx - 1].next_start {
                // We can only relocate if the previous dict has been relocated too
                Some(relocated_start) if relocated_start.segment_index >= 0 => {
                    // Relocate this dictionary based on the previous segment's next_start
                    vm.add_relocation_rule(self.trackers[tracker_idx].start, relocated_start)?;
                    let next_start =
                        (relocated_start + (dict_end - self.trackers[tracker_idx].start)?)?;
                    self.trackers[tracker_idx].next_start = Some(next_start);
                }
                _ => {
                    // Store the temporary next_start so we can properly finalize it later
                    self.trackers[tracker_idx].next_start = Some(dict_end);
                    return Ok(());
                }
            }
        }
        // Check if the next dict has been finalized but not relocated
        if let Some(next_dict) = self.trackers.get(tracker_idx + 1) {
            // Has been finalized but not relocated
            if next_dict.next_start.is_some_and(|r| r.segment_index < 0) {
                // As the next dict has already been finalized and the current one has been relocated
                // we know that this call will relocate the next dict
                self.finalize_segment(vm, next_dict.next_start.unwrap())?;
            }
        }
        Ok(())
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
        self.trackers[self.get_dict_infos_index(dict_end).ok()?].data.get(key).cloned()
    }
}

/// Helper object for the management of dict_squash hints.
#[derive(Default, Debug)]
pub struct DictSquashExecScope {
    /// A map from key to the list of indices accessing it, each list in reverse order.
    pub(crate) access_indices: HashMap<Felt252, Vec<Felt252>>,
    /// Descending list of keys.
    pub(crate) keys: Vec<Felt252>,
}

impl DictSquashExecScope {
    /// Returns the current key to process.
    pub fn current_key(&self) -> Option<Felt252> {
        self.keys.last().cloned()
    }

    /// Removes the current key, and its access indices. Should be called when only the
    /// last key access is in the corresponding indices list.
    pub fn pop_current_key(&mut self) -> Result<(), HintError> {
        let current_key = self.current_key().ok_or_else(|| {
            HintError::CustomHint("Failed to get current key".to_string().into_boxed_str())
        })?;
        let key_accesses = self.access_indices.remove(&current_key).ok_or_else(|| {
            HintError::CustomHint(format!("No key accesses for key {current_key}").into_boxed_str())
        })?;
        if !key_accesses.len().is_one() {
            return Err(HintError::CustomHint(
                "Key popped but not all accesses were processed.".to_string().into_boxed_str(),
            ));
        }
        self.keys.pop();
        Ok(())
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
