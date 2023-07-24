extern fn revoke_ap_tracking() implicits() nopanic;

/// Function to enforce that `Implicit` is used by a function calling it.
/// Note: This extern function is not mapped to a Sierra function, and all usages of it are removed
/// during compilation.
extern fn require_implicit<Implicit>() implicits(Implicit) nopanic;
