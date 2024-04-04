mod array;
mod assert;
mod consteval_int;
mod format;
mod panic;
mod print;
mod write;

use cairo_lang_plugins::get_base_plugins;

use self::assert::AssertMacro;
use self::format::FormatMacro;
use self::panic::PanicMacro;
use self::print::{PrintMacro, PrintlnMacro};
use self::write::{WriteMacro, WritelnMacro};
use super::inline_macros::array::ArrayMacro;
use super::inline_macros::consteval_int::ConstevalIntMacro;
use crate::plugin::PluginSuite;

/// Gets the default plugin suite to load into the Cairo compiler.
pub fn get_default_plugin_suite() -> PluginSuite {
    let mut suite = PluginSuite {
        plugins: get_base_plugins(),
        inline_macro_plugins: Default::default(),
        analyzer_plugins: Default::default(),
    };
    suite
        .add_inline_macro_plugin::<ArrayMacro>()
        .add_inline_macro_plugin::<AssertMacro>()
        .add_inline_macro_plugin::<ConstevalIntMacro>()
        .add_inline_macro_plugin::<FormatMacro>()
        .add_inline_macro_plugin::<PanicMacro>()
        .add_inline_macro_plugin::<PrintMacro>()
        .add_inline_macro_plugin::<PrintlnMacro>()
        .add_inline_macro_plugin::<WriteMacro>()
        .add_inline_macro_plugin::<WritelnMacro>();
    suite
}
