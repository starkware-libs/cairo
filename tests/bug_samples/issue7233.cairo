fn main() {
    let outer_var = 42;
    let closure_annotated = |i: u32| -> u32 { i + outer_var };
    println!("closure_annotated: {}", closure_annotated(1));
}
