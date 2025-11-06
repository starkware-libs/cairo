/// ### The `as` keyword.
/// Allows to alias an item when importing it with `use` statement.
///
/// ### Example
/// ```cairo
///  pub mod single_const {
///     pub const A: u8 = 1;
///  }
///
/// fn using_aliased_const() {
///     use single_const::A as B;
///     assert_eq!(B, 1);
/// }
/// ```
mod keyword_as {}

/// ### The `const` keyword.
/// Introduces a constant item or const generic parameter. Constants are evaluated at compile time.
///
/// ### Examples
///
/// - Module-level constant:
/// ```cairo
/// const TEN: u32 = 10;
/// fn times_ten(x: u32) -> u32 { x * TEN }
/// ```
///
/// - Const-generic parameter used by functions:
/// ```cairo
/// fn pad_with<const P: felt252>(x: felt252) -> felt252 { x + P }
/// ```
mod keyword_const {}

/// ### The `else` keyword.
/// Specifies an alternative branch for `if` or a fallback with `match` guards.
///
/// ### Example
/// ```cairo
/// fn is_positive(x: i32) -> bool {
///     if x >= 0 { true } else { false }
/// }
/// ```
mod keyword_else {}

/// ### The `enum` keyword.
/// Declares an enumeration type with named variants.
///
/// ### Example
/// ```cairo
/// #[derive(Copy, Drop)]
/// enum ResultU32 {
///     Ok: u32,
///     Err: felt252,
/// }
/// ```
mod keyword_enum {}

/// ### The `extern` keyword.
/// Declares external libfuncs or types provided by the compiler.
///
/// ### Example
/// ```cairo
/// extern fn array_new<T>() -> Array<T> nopanic;
/// ```
mod keyword_extern {}

/// ### The `false` keyword.
/// Boolean enum value representing logical false.
///
/// ### Example
/// ```cairo
/// fn default_bool() -> bool { false }
/// ```
mod keyword_false {}

/// ### The `fn` keyword.
/// Declares a function. Functions may specify implicits, panic behavior, generics, and attrs.
///
/// ### Example
/// ```cairo
/// fn add_u32(a: u32, b: u32) -> u32 { a + b }
/// ```
mod keyword_fn {}

/// ### The `if` keyword.
/// Begins a conditional branch.
///
/// ### Example
/// ```cairo
/// fn sign(x: felt252) -> felt252 {
///     if x == 0 {
///        0
///     } else {
///         if x > 0 { 1 } else { -1 }
///     }
/// }
/// ```
mod keyword_if {}

/// ### The `while` keyword.
/// Starts a conditional loop that runs while the condition is true.
///
/// ### Example
/// ```cairo
/// fn sum_first(n: u32) -> u32 {
///     let mut i = 0;
///     let mut acc = 0;
///     while i < n { acc = acc + i; i = i + 1; }
///     acc
/// }
/// ```
mod keyword_while {}

/// ### The `for` keyword.
/// Iteration construct over ranges or iterables.
///
/// ### Example
/// ```cairo
/// fn sum_range(n: u32) -> u32 {
///     let mut acc = 0;
///     for i in 0..n { acc = acc + i; }
///     acc
/// }
/// ```
mod keyword_for {}

/// ### The `loop` keyword.
/// Starts an infinite loop, typically exited with `break` or `return`.
///
/// ### Example
/// ```cairo
/// fn first_positive(xs: Array<felt252>) -> felt252 {
///     let mut i = 0;
///     loop {
///         if i >= xs.len() { return 0; }
///         let v = *xs.at(i).unwrap();
///         if v > 0 { return v; }
///         i = i + 1;
///     }
/// }
/// ```
mod keyword_loop {}

/// ### The `impl` keyword.
/// Introduces an implementation block for a trait or type.
///
/// ### Example
/// ```cairo
/// trait Doubler { fn double(self: @u32) -> u32; }
/// impl Doubling of Doubler {
///     fn double(self: @u32) -> u32 { *self + *self }
/// }
/// ```
mod keyword_impl {}

/// ### The `implicits` keyword.
/// Declares implicit parameters required by a function. These are usually passed automatically.
///
/// ### Example
/// ```cairo
/// extern fn check_in_u32_range(value: u32) -> (bool,) implicits(RangeCheck) nopanic;
/// ```
mod keyword_implicits {}

/// ### The `let` keyword.
/// Binds a new variable.
///
/// ### Example
/// ```cairo
/// fn square(x: u32) -> u32 { let y = x * x; y }
/// ```
mod keyword_let {}

/// ### The `macro` keyword.
/// Creates a declarative macro.
///
/// ### Example
/// ```cairo
/// macro add_one {
///    ($x:ident) => { $x + 1 };
/// }
/// ```
mod keyword_macro {}

/// ### The `match` keyword.
/// Pattern matching construct that selects a branch based on a value.
///
/// ### Example
/// ```cairo
/// fn to_bool(x: u32) -> bool {
///     match x { 0 => false, _ => true }
/// }
/// ```
mod keyword_match {}

/// ### The `mod` keyword.
/// Declares a module. Modules group items and control visibility.
///
/// ### Example
/// ```cairo
/// mod math_utils { pub fn add(a: u32, b: u32) -> u32 { a + b } }
/// ```
mod keyword_mod {}

/// ### The `mut` keyword.
/// Marks a binding or reference as mutable.
///
/// ### Example
/// ```cairo
/// fn count(n: u32) -> u32 {
///     let mut i = 0; while i < n { i = i + 1; } i
/// }
/// ```
mod keyword_mut {}

/// ### The `nopanic` keyword.
/// Marks a function as guaranteed not to panic. The compiler enforces no panicking paths.
///
/// ### Example
/// ```cairo
/// extern fn bool_to_felt252(a: bool) -> felt252 nopanic;
/// fn into_felt(b: bool) -> felt252 nopanic { bool_to_felt252(b) }
/// ```
mod keyword_nopanic {}

/// ### The `of` keyword.
/// Used in `impl Type of Trait` headers.
///
/// ### Example
/// ```cairo
/// trait Foo<T> { fn foo(self: @T) -> T; }
/// impl FooU32 of Foo<u32> { fn foo(self: @u32) -> u32 { *self + 1 } }
/// ```
mod keyword_of {}


/// ### The `ref` keyword.
/// Allows functions to mutate variables by passing them as a reference.
/// The value is implicitly copied and passed back to the user.
///
/// ### Example
/// ```cairo
/// fn push_four(ref ary: Array<felt252>) {
///     ary.append(4)
/// }
///
/// fn main() {
///     let mut ary = array![1, 2, 3];
///     push_four(ref ary);
///     assert!(ary == array![1, 2, 3, 4])
/// }
/// ```
mod keyword_ref {}

/// ### The `continue` keyword.
/// Skips to the next iteration of a loop.
///
/// ### Example
/// ```cairo
/// fn skip_even(n: u32) -> u32 {
///     let mut i = 0; let mut cnt = 0;
///     while i < n {
///         i = i + 1;
///         if i % 2 == 0 { continue; }
///         cnt = cnt + 1;
///     }
///     cnt
/// }
/// ```
mod keyword_continue {}

/// ### The `return` keyword.
/// Exits a function, optionally returning a value.
///
/// ### Example
/// ```cairo
/// fn clamp01(x: i32) -> u32 {
///     if x < 0 { return 0; }
///     if x > 1 { return 1; }
///     x
/// }
/// ```
mod keyword_return {}

/// ### The `break` keyword.
/// Exits a loop early. Can only be used inside a `loop` expression.
///
/// ### Example
/// ```cairo
/// fn first_gt(xs: Array<u32>, cmp: u32) -> Option<u32> {
///     let mut i = 0;
///     let v = loop {
///         if i >= xs.len() {
///             return None;
///         }
///
///         let v = *xs.at(i);
///         if v > cmp { break Some(v); }
///         i = i + 1;
///     };
///     v
/// }
/// ```
mod keyword_break {}

/// ### The `struct` keyword.
/// Declares a structure type with named members.
///
/// ### Example
/// ```cairo
/// struct Point { x: felt252, y: felt252 }
/// fn origin() -> Point { Point { x: 0, y: 0 } }
/// ```
mod keyword_struct {}

/// ### The `trait` keyword.
/// Declares a trait containing associated items to be implemented.
///
/// ### Example
/// ```cairo
/// trait Volume<T> { fn volume(self: @T) -> usize; }
/// impl ArrayVolume<T> of Volume<Array<T>> { fn volume(self: @Array<T>) -> usize { self.len() } }
/// ```
mod keyword_trait {}

/// ### The `true` keyword.
/// Boolean enum value representing logical true.
///
/// ### Example
/// ```cairo
/// fn yes() -> bool { true }
/// ```
mod keyword_true {}

/// ### The `type` keyword.
/// Declares a type alias.
///
/// ### Example
/// ```cairo
/// type usize = u32;
/// ```
mod keyword_type {}

/// ### The `use` keyword.
/// Imports items into the current scope.
///
/// ### Examples
/// ```cairo
/// use core::panic_with_felt252;
/// use super::traits::PartialEq;
/// ```
mod keyword_use {}

/// ### The `pub` keyword.
/// Makes an item public within its parent module (or crate).
///
/// ### Example
/// ```cairo
/// pub fn get_zero() -> u32 { 0 }
/// pub(crate) struct Pair { pub a: u32, b: u32 }
/// ```
mod keyword_pub {}
