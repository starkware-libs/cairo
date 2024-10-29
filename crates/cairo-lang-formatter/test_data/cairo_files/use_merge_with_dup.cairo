/// Testing basic functionality.
use a::a;
use a::a;
use a::b;
use a::c;
mod a;
use a::e;
use a::n;
use v::t;
use v::{a, b};
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
use a;
use a::a as c;
use a::b as d;
mod d;
/// Testing visibility.
pub use a::{a, b};
use e::e;
pub use a::b;
