#[cfg(test)]
#[path = "known_stack_test.rs"]
mod test;

use std::cmp::max;

use utils::ordered_hash_map::OrderedHashMap;
use utils::unordered_hash_set::UnorderedHashSet;

use crate::pre_sierra;

/// Represents the information known about the top of the stack at a given point in the code.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct KnownStack {
    /// A map from [sierra::ids::VarId] of variables that are located on the stack
    /// (e.g., `[ap - 2]`) to their index on the stack, relative to `offset`.
    ///
    /// A variable with index `i` is at the (`offset-i`)-th slot from the top of the stack.
    /// In particular, the top element has `i = offset - 1`.
    variables_on_stack: OrderedHashMap<sierra::ids::VarId, usize>,
    offset: usize,
}
impl KnownStack {
    /// Clears the known information about the stack.
    ///
    /// This is called where the change in the value of `ap` is not known at compile time.
    pub fn clear(&mut self) {
        self.offset = 0;
        self.variables_on_stack.clear();
    }

    /// Marks that the given variable appears on slot `idx` of the stack (note that `0` here means
    /// that the address is `ap`, and other indices will have larger addresses).
    pub fn insert(&mut self, var: sierra::ids::VarId, idx: usize) {
        self.variables_on_stack.insert(var, self.offset + idx);
    }

    /// Adds a value to the top of the stack, and advances `ap` accordingly (more precisely,
    /// `offset` is advanced by 1).
    pub fn push(&mut self, var: &sierra::ids::VarId) {
        self.insert(var.clone(), 0);
        self.offset += 1;
    }

    // If `src` is on the known stack, marks `dst` as located in the same cell.
    pub fn clone_if_on_stack(&mut self, src: &sierra::ids::VarId, dst: &sierra::ids::VarId) {
        if let Some(index_on_stack) = self.variables_on_stack.get(src).cloned() {
            self.variables_on_stack.insert(dst.clone(), index_on_stack);
        }
    }

    /// Updates offset according to the maximal index in `variables_on_stack`.
    /// This is the expected behavior after invoking a libfunc.
    pub fn update_offset_by_max(&mut self) {
        // `offset` is one more than the maximum of the indices in `variables_on_stack`
        // (or 0 if empty).
        self.offset = self.variables_on_stack.values().max().map(|idx| idx + 1).unwrap_or(0);
    }

    /// Removes the information known about the given variable.
    pub fn remove_variable(&mut self, var: &sierra::ids::VarId) {
        self.variables_on_stack.swap_remove(var);
    }

    // Checks if there exists a prefix of `push_values`, that is already on the top of the stack.
    // Returns the prefix size if exists, and 0 otherwise.
    pub fn compute_on_stack_prefix_size(&self, push_values: &[pre_sierra::PushValue]) -> usize {
        if let Some(index_on_stack) = self.variables_on_stack.get(&push_values[0].var) {
            // Compute the prefix size, if exists.
            let prefix_size = self.offset - index_on_stack;
            if prefix_size > push_values.len() {
                return 0;
            }
            // Check if this is indeed a prefix.
            let is_prefix = (1..prefix_size).all(|i| {
                self.variables_on_stack.get(&push_values[i].var).cloned()
                    == Some(index_on_stack + i)
            });
            if is_prefix {
                return prefix_size;
            }
        }
        0
    }

    /// Merges two stacks.
    ///
    /// If a variable appears in both stacks in the same place (relative to the top of the two
    /// stacks), it will appear in the new stack.
    /// The resulting stack must be continuous (otherwise, one also needs to check that the sizes of
    /// the "holes" are identical).
    ///
    /// For example, merging the stacks [0, 1, 2, 3, 4] and [1, 9, 3, 4] (where the last element in
    /// the top) will yield [3, 4] (1 will not be included because of the hole).
    #[allow(dead_code)]
    pub fn merge_with(&self, other: &Self) -> Self {
        // Choose the new offset to be the maximum of the input offsets. This is somewhat arbitrary.
        let new_offset = max(self.offset, other.offset);

        // Prepare a list of indices that may appear in the merged stack, relative to the top of the
        // stack. Here, `1` means the top element.
        let mut indices = UnorderedHashSet::<usize>::default();
        // Prepares a temporary map of variables. This map will later be filtered when the actual
        // size of the merged stack is known.
        let mut tmp_variables_on_stack = OrderedHashMap::<sierra::ids::VarId, usize>::default();
        for (var, self_index) in self.variables_on_stack.iter() {
            if let Some(other_index) = other.variables_on_stack.get(var) {
                assert!(
                    *self_index < self.offset,
                    "Stack index ({self_index}) must be < offset ({})",
                    self.offset
                );
                let self_rel = self.offset - self_index;

                assert!(
                    *other_index < other.offset,
                    "Stack index ({other_index}) must be < offset ({})",
                    other.offset
                );
                let other_rel = other.offset - other_index;

                if self_rel == other_rel {
                    indices.insert(self_rel);
                    tmp_variables_on_stack.insert(var.clone(), new_offset - self_rel);
                }
            }
        }

        // Find the longest continuous common suffix.
        let suffix_size = if let Some(suffix_size) =
            (0..new_offset + 1).find(|index| !indices.contains(&(index + 1)))
        {
            suffix_size
        } else {
            panic!("Internal compiler error.");
        };

        KnownStack {
            variables_on_stack: tmp_variables_on_stack
                .into_iter()
                .filter(|(_var, index)| new_offset - index <= suffix_size)
                .collect(),
            offset: new_offset,
        }
    }
}
