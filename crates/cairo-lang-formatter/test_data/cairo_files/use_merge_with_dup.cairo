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
/// Testing attributes.
mod b;
use a::a;
#[cfg(test)]
use a::a;
use a::b;
use a::c;
/// Testing aliases.
mod c;
use a;
use a::a as c;
use a::b as d;
/// Testing visibility settings.
mod d;
pub use a::{a, b};
use e::e;
pub use a::b;
mod e;
use a::b;
use a::b::c;
use a::b::c::d;
use a::b::c::d::e;
use a::b::c::d::f::g;
/// Testing not merging with trivia.
mod t;
// This is a comment for a.
use a;
use b;
