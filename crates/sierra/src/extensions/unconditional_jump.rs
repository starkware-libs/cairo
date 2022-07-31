use crate::extensions::*;

struct UnconditionalJumpExtension {}

impl ExtensionImplementation for UnconditionalJumpExtension {
    fn simulate(
        &self,
        _tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), Error> {
        validate_mem_sizes(&inputs, [])?;
        Ok((inputs, 0))
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 1] {
    [("jump".to_string(), Box::new(UnconditionalJumpExtension {}))]
}
