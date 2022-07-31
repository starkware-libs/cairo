use super::*;

enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

struct OpExtension {
    op: Op,
}

impl NonBranchExtensionImplementation for OpExtension {
    fn simulate(
        &self,
        _tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, ExtensionError> {
        validate_mem_sizes(&inputs, [1, 1])?;
        Ok(vec![vec![match self.op {
            Op::Add => inputs[0][0] + inputs[1][0],
            Op::Sub => inputs[0][0] - inputs[1][0],
            Op::Mul => inputs[0][0] * inputs[1][0],
            Op::Div => inputs[0][0] / inputs[1][0],
            Op::Mod => inputs[0][0] % inputs[1][0],
        }]])
    }
}

struct OpWithConstExtension {
    op: Op,
}

impl NonBranchExtensionImplementation for OpWithConstExtension {
    fn simulate(
        &self,
        tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, ExtensionError> {
        let c = single_value_arg(tmpl_args)?;
        validate_mem_sizes(&inputs, [1])?;
        Ok(vec![vec![match self.op {
            Op::Add => inputs[0][0] + c,
            Op::Sub => inputs[0][0] - c,
            Op::Mul => inputs[0][0] * c,
            Op::Div => inputs[0][0] / c,
            Op::Mod => inputs[0][0] % c,
        }]])
    }
}

struct IgnoreExtension {}

impl NonBranchExtensionImplementation for IgnoreExtension {
    fn simulate(
        &self,
        _tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, ExtensionError> {
        validate_mem_sizes(&inputs, [1])?;
        Ok(vec![])
    }
}

struct DuplicateExtension {}

impl NonBranchExtensionImplementation for DuplicateExtension {
    fn simulate(
        &self,
        _tmpl_args: &[TemplateArg],
        mut inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, ExtensionError> {
        validate_mem_sizes(&inputs, [1])?;
        Ok(vec![inputs[0].clone(), inputs.remove(0)])
    }
}

struct ConstantExtension {}

impl NonBranchExtensionImplementation for ConstantExtension {
    fn simulate(
        &self,
        tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, ExtensionError> {
        validate_mem_sizes(&inputs, [])?;
        Ok(vec![vec![single_value_arg(tmpl_args)?]])
    }
}

struct JumpNzExtension {}

impl ExtensionImplementation for JumpNzExtension {
    fn simulate(
        &self,
        _tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<(Vec<Vec<i64>>, usize), ExtensionError> {
        validate_mem_sizes(&inputs, [1])?;
        if inputs[0][0] != 0 { Ok((inputs, 0)) } else { Ok((vec![], 1)) }
    }
}

struct UnwrapNzExtension {}

impl NonBranchExtensionImplementation for UnwrapNzExtension {
    fn simulate(
        &self,
        _tmpl_args: &[TemplateArg],
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, ExtensionError> {
        validate_mem_sizes(&inputs, [1])?;
        Ok(inputs)
    }
}

pub(super) fn extensions() -> [(String, ExtensionBox); 13] {
    [
        ("int_add".to_string(), wrap_non_branch(Box::new(OpExtension { op: Op::Add }))),
        ("int_sub".to_string(), wrap_non_branch(Box::new(OpExtension { op: Op::Sub }))),
        ("int_mul".to_string(), wrap_non_branch(Box::new(OpExtension { op: Op::Mul }))),
        (
            "int_add_const".to_string(),
            wrap_non_branch(Box::new(OpWithConstExtension { op: Op::Add })),
        ),
        (
            "int_sub_const".to_string(),
            wrap_non_branch(Box::new(OpWithConstExtension { op: Op::Sub })),
        ),
        (
            "int_mul_const".to_string(),
            wrap_non_branch(Box::new(OpWithConstExtension { op: Op::Mul })),
        ),
        (
            "int_div_const".to_string(),
            wrap_non_branch(Box::new(OpWithConstExtension { op: Op::Div })),
        ),
        (
            "int_mod_const".to_string(),
            wrap_non_branch(Box::new(OpWithConstExtension { op: Op::Mod })),
        ),
        ("int_ignore".to_string(), wrap_non_branch(Box::new(IgnoreExtension {}))),
        ("int_dup".to_string(), wrap_non_branch(Box::new(DuplicateExtension {}))),
        ("int_const".to_string(), wrap_non_branch(Box::new(ConstantExtension {}))),
        ("int_jump_nz".to_string(), Box::new(JumpNzExtension {})),
        ("int_unwrap_nz".to_string(), wrap_non_branch(Box::new(UnwrapNzExtension {}))),
    ]
}
