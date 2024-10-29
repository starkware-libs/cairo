/// Testing basic functionality.
use a::a;
use a::a;
use a::c;
use a::b;
mod a;
use a::e;
use v::t;
use v::{b, a};
use a::n;
mod b;
use z::a;
mod b;
/// Testing attributes.
use a::a;
#[cfg(test)]
use a::a;
use a::b;
use a::c;
mod c;
/// Testing aliases.
use a::a as c;
use a;
use a::b as d;
mod d;
/// Testing visibility.
pub use a::{a, b};
use e::e;
pub use a::c;
