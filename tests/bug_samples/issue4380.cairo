fn get_value() -> Option<u8> {
    Some(42)
}

fn process_value() -> Result<u8, felt252> {
    let value = get_value().ok_or('no value')?;
    assert!(value < 69, "fail");
    Ok(value)
}
