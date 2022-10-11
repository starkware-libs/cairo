use crate::lower::Lowerer;
use crate::objects::VariableId;

/// Wrapper around VariableId, guaranteeing that the variable is alive.
/// Thus, it does not implement copy nor clone.
pub struct OwnedVariable(VariableId);
impl OwnedVariable {
    // TODO(spapini): Remove once statements are refactored.
    pub fn var_id(&self) -> VariableId {
        self.0
    }

    /// Duplicates the variable if it is duplicatable.
    pub fn try_duplicate(&self, lowerer: &Lowerer<'_>) -> Option<Self> {
        if lowerer.variables[self.0].duplicatable { Some(OwnedVariable(self.0)) } else { None }
    }
}
