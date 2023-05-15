use cairo_lang_sierra::extensions::cheatcodes::CheatcodesConcreteLibFunc;

use crate::objects::ConstCost;

pub fn cheatcodes_libfunc_cost_base(libfunc: &CheatcodesConcreteLibFunc) -> Vec<ConstCost> {
    let steps = |value| ConstCost { steps: value, ..Default::default() };
    match libfunc {
        CheatcodesConcreteLibFunc::Declare(_) => vec![steps(2), steps(2)],
        CheatcodesConcreteLibFunc::DeclareCairo0(_) => vec![steps(2), steps(2)],
        CheatcodesConcreteLibFunc::StartRoll(_) => vec![steps(1), steps(1)],
        CheatcodesConcreteLibFunc::StopRoll(_) => vec![steps(1), steps(1)],
        CheatcodesConcreteLibFunc::StartWarp(_) => vec![steps(1), steps(1)],
        CheatcodesConcreteLibFunc::StopWarp(_) => vec![steps(1), steps(1)],
        CheatcodesConcreteLibFunc::StartPrank(_) => vec![steps(1), steps(1)],
        CheatcodesConcreteLibFunc::StopPrank(_) => vec![steps(1), steps(1)],
        CheatcodesConcreteLibFunc::Invoke(_) => vec![steps(3), steps(3)],
        CheatcodesConcreteLibFunc::MockCall(_) => vec![steps(1), steps(1)],
        CheatcodesConcreteLibFunc::Deploy(_) => vec![steps(3), steps(3)],
        CheatcodesConcreteLibFunc::Prepare(_) => vec![steps(2), steps(2)],
        CheatcodesConcreteLibFunc::Call(_) => vec![steps(3), steps(3)],
        CheatcodesConcreteLibFunc::Print(_) => vec![steps(1)],
    }
}
