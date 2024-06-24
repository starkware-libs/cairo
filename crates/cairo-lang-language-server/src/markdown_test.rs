use indoc::indoc;

use super::Markdown;

#[test]
fn test_convert_fenced_code_blocks_to_cairo() {
    let input = indoc! {r#"
        This documentation block tests markdown manipulation performed by LS.

        ```
        fn this_is_cairo_code_block() {}
        ```

        Lorem ipsum.

           this is nested
           ```
           code block
           ```

        Lorem ipsum.

        ```text
        this is text code block
        ```

        ```cairo
        just checking if past fences did not break anything
        ```
    "#};

    let expected = indoc! {r#"
        This documentation block tests markdown manipulation performed by LS.

        ```cairo
        fn this_is_cairo_code_block() {}
        ```

        Lorem ipsum.

           this is nested
           ```
           code block
           ```

        Lorem ipsum.

        ```text
        this is text code block
        ```

        ```cairo
        just checking if past fences did not break anything
        ```
    "#};

    let mut actual = Markdown::from(input);
    actual.convert_fenced_code_blocks_to_cairo();
    actual.ensure_trailing_newline();
    assert_eq!(expected, actual.to_string());
}
