use crate::function_generator_test_utils::test_function_generator;

cairo_lang_test_utils::test_file_test!(
    function_generator,
    "src/function_generator_test_data",
    {
        inline: "inline",
        match_: "match",
        simple: "simple",
        literals: "literals",

    },
    test_function_generator
);
