#![no_std]
#![no_main]

use core::panic::PanicInfo;

/// This function is called on panic.
#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[no_mangle]
pub extern "C" fn _start() -> ! {
    loop {}
}

#[global_allocator]
// NOTE: this should be initialized before use
static ALLOC: esp_alloc::EspHeap = esp_alloc::EspHeap::empty();

#[expect(unused_imports)]
use cairo_lang_casm;
#[expect(unused_imports)]
use cairo_lang_utils;
