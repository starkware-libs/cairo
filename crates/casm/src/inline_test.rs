use indoc::indoc;
use itertools::join;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::instructions::Instruction;
use crate::{casm, deref};

#[test]
fn test_assert() {
    let x = 1;
    let y = deref!([fp + 5]);
    let z = 5;

    let ctx = casm! {
        [fp - 5] = x, ap++;
        [fp - 5] = [ap + 1] + [fp - 5], ap++;
        [fp + 5] = [ap + 1] + 2;
        [ap + 5] = [[ap + 1] + 2];
        [ap + 5] = [[ap - 1] + 2];
        [ap + 5] = [[ap + 1] - 2];
        [ap + 5] = [[ap] + 2];
        [ap + 5] = [[ap + 1]];
        [ap + 5] = [[ap]];
        [ap] = [ap + 1] * [fp - 5];
        %{ memory[ap + 5] = segments.add() %}
        [fp - 5] = [ap + 1] * z;
        [fp - 5] = [ap + 1] * y;
        %{ memory[ap + 0] = memory[fp - 3] < 45 %}
        %{ memory[ap + 0] = 13 < memory[fp + 9] %}
        [fp - 5] = 1, ap++;
        [fp - 5] = [ap + 1];
        %{ memory[ap + 0] = memory[ap + 9] < memory[fp + 9] %}
        jmp 123 if [ap + 17] != 0;
        jmp rel [fp - 19] if [ap + 17] != 0;
        %{ (memory[ap + 0], memory[ap + 1]) = divmod(memory[ap + 9], memory[fp + 9]) %}
        %{ (memory[ap + 0], memory[ap + 1]) = divmod(50, memory[fp + 9]) %}
        %{ (memory[ap + 0], memory[ap + 1]) = divmod(memory[ap + 9], 2) %}
        call abs 5, ap++;
        call rel y, ap++;
        ret;
    };

    let code = join(ctx.instructions.iter().map(Instruction::to_string), "\n");
    assert_eq!(
        code,
        indoc! {"
            [fp + -5] = 1, ap++
            [fp + -5] = [ap + 1] + [fp + -5], ap++
            [fp + 5] = [ap + 1] + 2
            [ap + 5] = [[ap + 1] + 2]
            [ap + 5] = [[ap + -1] + 2]
            [ap + 5] = [[ap + 1] + -2]
            [ap + 5] = [[ap + 0] + 2]
            [ap + 5] = [[ap + 1] + 0]
            [ap + 5] = [[ap + 0] + 0]
            [ap + 0] = [ap + 1] * [fp + -5]
            %{ memory[ap + 5] = segments.add() %}
            [fp + -5] = [ap + 1] * 5
            [fp + -5] = [ap + 1] * [fp + 5]
            %{ memory[ap + 0] = memory[fp + -3] < 45 %}
            %{ memory[ap + 0] = 13 < memory[fp + 9] %}
            [fp + -5] = 1, ap++
            [fp + -5] = [ap + 1]
            %{ memory[ap + 0] = memory[ap + 9] < memory[fp + 9] %}
            jmp rel 123 if [ap + 17] != 0
            jmp rel [fp + -19] if [ap + 17] != 0
            %{ (memory[ap + 0], memory[ap + 1]) = divmod(memory[ap + 9], memory[fp + 9]) %}
            %{ (memory[ap + 0], memory[ap + 1]) = divmod(50, memory[fp + 9]) %}
            %{ (memory[ap + 0], memory[ap + 1]) = divmod(memory[ap + 9], 2) %}
            call abs 5, ap++
            call rel [fp + 5], ap++
            ret"}
    );
}
