fn test(expression: String, args: Vec<String>) -> String {
    "hello".to_string()
}

macro_rules! quote_format {
  ($literal:expr, $($arg:expr),*) => {{
      let positional_arguments: Vec<_> = $literal.matches("{}").collect();
      let positional_arguments_number = positional_arguments.len();
      let args_num: usize = [$($arg),*].len();

      assert!(
        positional_arguments_number >= args_num,
          "Too many arguments provided for the number of positional arguments. Positional arguments: {}, arguments: {}", positional_arguments_number, args_num
      );
      assert!(
        args_num >= positional_arguments_number,
          "Too many positional arguments for provided arguments. Positional arguments: {}, arguments: {}", positional_arguments_number, args_num
      );

      test($literal.to_string(), vec![$($arg.to_string()),*])
  }};
}
