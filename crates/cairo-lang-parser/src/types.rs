use std::fmt::Display;

use cairo_lang_filesystem::span::TextOffset;

pub trait TokenStream: Display {
    fn get_start_offset(&self) -> Option<TextOffset>;
}
