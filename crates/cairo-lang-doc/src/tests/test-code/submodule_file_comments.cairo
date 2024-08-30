//! This is a submodule regarding the module_level_comments.

//! It's used to make sure crate / module level comments are parsed in a correct way.
//! Testing purposes only!

mod inner_sub_module {
    //! This comment just proves that it won't be considered as a file-module comment. It just refers to the inner_sub_module

    fn hello() {
        println!("Hello!");
    }
}
