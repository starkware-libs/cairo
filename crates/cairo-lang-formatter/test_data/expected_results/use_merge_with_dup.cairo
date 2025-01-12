use a::{a, a, b, c};
mod a;
use a::{e, n};
use v::{a, b, t};
mod b;
use z::a;
/// Testing attributes.
mod b;
#[cfg(test)]
use a::a;
use a::{a, b, c};
/// Testing aliases.
mod c;
use a;
use a::{a as c, b as d};
/// Testing visibility settings.
mod d;
pub use a::{a, b, b};
use e::e;
mod e;
use a::b;
use a::b::c;
use a::b::c::d;
use a::b::c::d::e;
use a::b::c::d::f::g;
/// Testing not merging with trivia.
mod t;
// This is a comment for a::b.
use a::b;
use a::{c, c};
// Testing wildcard.
mod w;
use a::{*, b, b, c};
use d::{*, e};
