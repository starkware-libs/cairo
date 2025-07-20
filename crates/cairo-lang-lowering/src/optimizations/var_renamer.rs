use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use crate::utils::Rebuilder;
use crate::{BlockId, VariableId};

/// Utility for renaming variables.
/// Support recursive renaming, e.g. if A is renamed to B, and B is renamed to C, then A is renamed
/// to C.
#[derive(Default)]
pub struct VarRenamer<'db> {
    pub renamed_vars: UnorderedHashMap<VariableId<'db>, VariableId<'db>>,
}

impl<'db> Rebuilder<'db> for VarRenamer<'db> {
    fn map_var_id(&mut self, var: VariableId<'db>) -> VariableId<'db> {
        let Some(mut new_var_id) = self.renamed_vars.get(&var).cloned() else {
            return var;
        };
        while let Some(new_id) = self.renamed_vars.get(&new_var_id) {
            assert_ne!(new_var_id, *new_id);
            new_var_id = *new_id;
        }

        self.renamed_vars.insert(var, new_var_id);
        new_var_id
    }

    fn map_block_id(&mut self, block: BlockId) -> BlockId {
        block
    }
}
