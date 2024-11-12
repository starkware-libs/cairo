#[test]
fn test() {
    let db = SimpleParserDatabase::default();
    let root_syntax_node = db
        .parse_virtual(indoc! {"
      fn test_function() {}
    "})
        .unwrap();

    let a = quote_format!(db.upcast(), "test {}", root_syntax_node);
    println!("result: {:?}", a.tokens);
}
