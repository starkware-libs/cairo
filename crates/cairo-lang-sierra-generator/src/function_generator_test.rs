use crate::function_generator_test_utils::test_function_generator;

cairo_lang_test_utils::test_file_test!(
    function_generator,
    "src/function_generator_test_data",
    {
        match_: "match",
        simple: "simple",
    },
    test_function_generator
);
