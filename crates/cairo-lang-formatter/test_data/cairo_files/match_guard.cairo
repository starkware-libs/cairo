fn classify(x: u32, flag: bool) -> u32 {
    match x {
        0 =>   1,
        1   if   flag => 2,
        10 | 20   if x > 15 && flag => 3,
        y if y > 100 && flag || y < 50 && !flag && y != 7 && y != 9 && y != 11 && y != 13 => 4,
        z if z > 1000 && flag && z != 7 && z != 9 && z != 11 && z != 13 && z != 15 && z != 17 && z != 19 => 6,
        _ => 5,
    }
}
