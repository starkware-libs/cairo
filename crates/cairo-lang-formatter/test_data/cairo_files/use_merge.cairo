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
pub use a::c;
pub use a::{a, b};
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
use a::c;
use a::d;
// Testing wildcard.
mod w;
use a::a;
use a::{c, b};
use a::*;
use d::{e, *};
// Testing not merging the top level.
use x;
use y;
// Testing the handling of crate and super.
mod z;
use a::b;
use a::c;
use a::d::e;
use a::f::g::h;
use crate::a;
use crate::b;
use b;
use super::v;
use crate::bl;
