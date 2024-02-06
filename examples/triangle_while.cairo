fn triangle_while(n: felt252) -> felt252 {
    let mut arr = array![];
    let mut i = 1;
    while i != n + 1 {
        arr.append(i);
        i += 1;
    };
    let mut sum = 0;
    while let Option::Some(x) = arr.pop_front() {
        sum += x;
    };
    sum
}
