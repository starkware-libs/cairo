use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::instructions::*;
use crate::operand::*;
use crate::{casm, deref};

#[test]
fn test_assert() {
    let x = ResOperand::Immediate(ImmediateOperand { value: 1 });
    let y = deref!([fp + 5]);

    let ctx = casm! {
        [fp - 5] = x, ap++;
        [fp - 5] = [ap + 1] + [fp - 5], ap++;
        [fp + 5] = [ap + 1] + 2;
        [ap] = [ap + 1] * [fp - 5];
        [fp - 5] = [ap + 1] * 2;
        [fp - 5] = [ap + 1] * y;
        [fp - 5] = 1, ap++;
        start:
        [fp - 5] = [ap + 1];
        call abs y, ap++;
        call rel 5, ap++;
        call rel start, ap++;
    };

    let code = ctx.instructions.iter().map(Instruction::to_string).collect::<Vec<_>>().join("\n");
    assert_eq!(
        code,
        indoc! {"
            [fp + -5] = 1, ap++
            [fp + -5] = [ap + 1] + [fp + -5], ap++
            [fp + 5] = [ap + 1] + 2
            [ap + 0] = [ap + 1] + [fp + -5]
            [fp + -5] = [ap + 1] + 2
            [fp + -5] = [ap + 1] + [fp + 5]
            [fp + -5] = 1, ap++
            [fp + -5] = [ap + 1]
            call abs [fp + 5], ap++
            call rel 5, ap++
            call rel -4, ap++"}
    );
}
