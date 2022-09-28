use utils::ordered_hash_map::OrderedHashMap;

use crate::pre_sierra;

/// Represents the information known about the top of the stack at a given point in the code.
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
    pub fn default() -> KnownStack {
        Self { variables_on_stack: OrderedHashMap::default(), offset: 0 }
    }

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
}
