fn fib(a: felt252, b: felt252, n: felt252) -> felt252 {
    match n {
        0 => a,
        _ => fib(b, a + b, n - 1),
    }
}

#[cfg(test)]
mod tests {
    use super::fib;

    #[test]
    fn test_fib_0() {
        assert!(fib(0, 1, 0) == 0);
    }
    
    #[test]
    fn test_fib_5() {
        assert!(fib(0, 1, 5) == 5);
    }
    
    #[test]
    fn test_fib_10() {
        assert!(fib(0, 1, 10) == 55);
    }
}
