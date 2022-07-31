use super::*;

struct StoreExtension {}

impl NonBranchExtensionImplementation for StoreExtension {
    fn simulate(
        &self,
        _tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        validate_mem_sizes(&inputs, [1])?;
        Ok(inputs)
    }
}

struct RenameExtension {}

impl NonBranchExtensionImplementation for RenameExtension {
    fn simulate(
        &self,
        _tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        Ok(inputs)
    }
}

struct MoveExtension {}

impl NonBranchExtensionImplementation for MoveExtension {
    fn simulate(
        &self,
        _tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        Ok(inputs)
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 3] {
    [
        ("store".to_string(), wrap_non_branch(Box::new(StoreExtension {}))),
        ("rename".to_string(), wrap_non_branch(Box::new(RenameExtension {}))),
        ("move".to_string(), wrap_non_branch(Box::new(MoveExtension {}))),
    ]
}
