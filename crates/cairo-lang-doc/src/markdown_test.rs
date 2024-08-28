use indoc::indoc;

use super::cleanup_doc_markdown;

#[test]
fn fenced_code_blocks() {
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

    let actual = cleanup_doc_markdown(input.to_owned());
    assert_eq!(expected, actual);
}
