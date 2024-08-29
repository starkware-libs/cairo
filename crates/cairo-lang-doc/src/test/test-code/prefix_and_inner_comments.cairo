/// Main function comment outside.
fn main() {
    //! Main function comment inside.
    println!("main");
}

/// Trait containing abc function.
trait TraitTest {
    /// abc function returning u32.
    /// Default impl of abc TraitTest function.
    fn abc() -> u32 {
        //! Default impl of abc TraitTest function inner comment.
        println!("default impl");
        32
    }
}

/// Implementation of TraitTest's abc function.
//! Additional comment for TraitTestImpl.
impl TraitTestImpl of TraitTest {
    /// Default impl of abc TraitTest function.
    fn abc() -> u32 {
        //! Default impl of abc TraitTest function inner comment.
        println!("abc");
        32
    }
}

/// Test module used to check if the documentation is being attachted to the nodes correctly.
//! Additional comment for test_module.
pub mod test_module {
    //! Test module used to check if the documentation is being attachted to the nodes correctly.
    /// Just a function outside the test_module.
    pub fn inner_test_module_function() {
        //! Just a function inside the test_module.
        println!("inside inner test module inner function");
    }
}

/// Point struct representing a point in a 2d space.
struct Point {
    /// X coordinate.
    x: u32,
    /// Y coordinate.
    y: u32
}

/// Answer Enum representing an answer to a yes/no question.
enum Answer {
    /// Yes answer variant.
    Yes,
    /// No answer variant.
    No
}
