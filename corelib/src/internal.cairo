pub extern fn revoke_ap_tracking() implicits() nopanic;

/// Function to enforce that `Implicit` is used by a function calling it.
/// Note: This extern function is not mapped to a Sierra function, and all usages of it are removed
/// during compilation.
pub extern fn require_implicit<Implicit>() implicits(Implicit) nopanic;

extern type index_enum_type<const NUM_VARIANTS: felt252>;

extern type BoundedInt<const MIN: felt252, const MAX: felt252>;

extern type S<const N: felt252>;
extern fn bar()->S<{3 + 8}> nopanic;
const k : felt252 = 98;
mod A {
    fn f() {
        let x: super::S<{ super::k + 9 }> = super::bar();
    }
}