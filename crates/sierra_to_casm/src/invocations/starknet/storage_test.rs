use itertools::Itertools;

use crate::invocations::test_utils::compile_libfunc;
use crate::ref_expr;

// TODO(yuval): add a test for storage_read.
// TODO(yuval): move tests to test infrastructure.

#[test]
fn test_storage_write() {
    assert_eq!(
        compile_libfunc(
            "storage_write_syscall",
            vec![
                ref_expr!([fp + 1]),
                ref_expr!([fp + 2]),
                ref_expr!([ap + 5]),
                ref_expr!([ap + 6])
            ],
        )
        .instructions
        .iter()
        .map(|y| y.to_string())
        .join("\n"),
        indoc::indoc! {"
            [ap + 0] = 8038072152842849266968829064307, ap++
            [ap + -1] = [[fp + 2] + 0]
            [fp + 1] = [[fp + 2] + 1]
            [ap + 4] = [[fp + 2] + 2]
            [ap + 5] = [[fp + 2] + 3]
            %{ syscall_handler.syscall(segments=segments, syscall_ptr=[fp + 2] + 0) %}
            [ap + 0] = [[fp + 2] + 5], ap++
            jmp rel 0 if [ap + -1] != 0"}
    );
}
