use core::blake::blake2s_compress;

// Calculates fib...
#[executable]
fn test_blake2s() {
     let state = BoxTrait::new([0_u32; 8]);
    let msg = BoxTrait::new([0_u32; 16]);
    let byte_count = 64_u32;

    let _res = blake2s_compress(state, byte_count, msg).unbox();

    // assert_eq!(
    //     res,
    //     [
    //         3893814314, 2107143640, 4255525973, 2730947657, 3397056017, 3710875177, 3168346915,
    //         365144891,
    //     ],
    // );
}
