use cairo_lang_sierra::extensions::cheatcodes::CheatcodesConcreteLibFunc;

use crate::core_libfunc_cost_base::CostOperations;

pub fn cheatcodes_libfunc_cost_base<Ops: CostOperations>(
    ops: &mut Ops,
    libfunc: &CheatcodesConcreteLibFunc,
) -> Vec<Ops::CostType> {
    match libfunc {
        CheatcodesConcreteLibFunc::Declare(_) => vec![ops.steps(2), ops.steps(2)],
        CheatcodesConcreteLibFunc::DeclareCairo0(_) => vec![ops.steps(2), ops.steps(2)],
        CheatcodesConcreteLibFunc::StartRoll(_) => vec![ops.steps(1), ops.steps(1)],
        CheatcodesConcreteLibFunc::StopRoll(_) => vec![ops.steps(1), ops.steps(1)],
        CheatcodesConcreteLibFunc::StartWarp(_) => vec![ops.steps(1), ops.steps(1)],
        CheatcodesConcreteLibFunc::StopWarp(_) => vec![ops.steps(1), ops.steps(1)],
        CheatcodesConcreteLibFunc::StartPrank(_) => vec![ops.steps(1), ops.steps(1)],
        CheatcodesConcreteLibFunc::StopPrank(_) => vec![ops.steps(1), ops.steps(1)],
        CheatcodesConcreteLibFunc::Invoke(_) => vec![ops.steps(3), ops.steps(3)],
        CheatcodesConcreteLibFunc::MockCall(_) => vec![ops.steps(1), ops.steps(1)],
        CheatcodesConcreteLibFunc::Deploy(_) => vec![ops.steps(3), ops.steps(3)],
        CheatcodesConcreteLibFunc::Prepare(_) => vec![ops.steps(2), ops.steps(2)],
        CheatcodesConcreteLibFunc::Call(_) => vec![ops.steps(3), ops.steps(3)],
        CheatcodesConcreteLibFunc::Print(_) => vec![ops.steps(1)],
    }
}
