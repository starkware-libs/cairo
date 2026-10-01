fn classify(x: u32, flag: bool) -> u32 {
    match x {
        0 =>   1,
        1 .. 5   if   flag => 2,
        5 ..=  9   => 3,
        10 | 20   if x > 15 && flag => 4,
        _ => 5,
    }
}
