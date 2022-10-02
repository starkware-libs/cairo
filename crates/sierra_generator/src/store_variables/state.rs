use sierra::extensions::lib_func::OutputVarInfo;
use sierra::extensions::OutputVarReferenceInfo;
use utils::ordered_hash_map::OrderedHashMap;

use super::known_stack::KnownStack;

/// Represents information known about the state of the variables.
/// For example, which variable contains a deferred value and which variable is on the stack.
#[derive(Clone, Debug, Default)]
pub struct State {
    /// A map from [sierra::ids::VarId] of a deferred reference
    /// (for example, `[ap - 1] + [ap - 2]`) to its type.
    pub deferred_variables: OrderedHashMap<sierra::ids::VarId, sierra::ids::ConcreteTypeId>,
    /// The information known about the top of the stack.
    pub known_stack: KnownStack,
}
impl State {
    /// Registers output variables of a libfunc. See `add_output`.
    pub fn register_outputs(
        &mut self,
        results: &[sierra::ids::VarId],
        output_infos: &[OutputVarInfo],
    ) {
        for (var, output_info) in itertools::zip_eq(results, output_infos) {
            self.register_output(var.clone(), output_info);
        }
        // Update `known_stack_size`. It is one more than the maximum of the indices in
        // `variables_on_stack` (or 0 if empty).
        self.known_stack.update_offset_by_max();
    }

    /// Register an output variable of a libfunc.
    ///
    /// If the variable is marked as Deferred output by the libfunc, it is added to
    /// `self.deferred_variables`.
    fn register_output(&mut self, res: sierra::ids::VarId, output_info: &OutputVarInfo) {
        self.deferred_variables.swap_remove(&res);
        self.known_stack.remove_variable(&res);
        match output_info.ref_info {
            OutputVarReferenceInfo::Deferred => {
                self.deferred_variables.insert(res, output_info.ty.clone());
            }
            OutputVarReferenceInfo::NewTempVar { idx } => {
                self.known_stack.insert(res, idx);
            }
            OutputVarReferenceInfo::SameAsParam { .. }
            | OutputVarReferenceInfo::NewLocalVar
            | OutputVarReferenceInfo::Const => {}
        }
    }

    /// Clears the known information about the stack.
    ///
    /// This is called where the change in the value of `ap` is not known at compile time.
    pub fn clear_known_stack(&mut self) {
        self.known_stack.clear();
    }
}
