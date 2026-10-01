fn classify(x: u32, flag: bool) -> u32 {
    match x {
        0 | 1 if flag => 1,
        y if y > 5 && flag => 2,
        _ => 3,
    }
}
