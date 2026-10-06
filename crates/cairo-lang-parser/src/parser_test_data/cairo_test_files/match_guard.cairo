fn classify(x: u32, flag: bool) -> u32 {
    match x {
        0 | 1 if flag => 1,
        y if y > 5 && flag => 2,
        _ => 3,
    }
}

fn complex_guards(x: u32, p: Point) -> u32 {
    match x {
        0 if p == Point { a: 1 } => 1,
        1 if {
            x > 1
        } => 2,
        2 if if p.a > 1 {
            true
        } else {
            false
        } => 3,
        _ => 4,
    }
}
