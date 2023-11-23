use starknet::get_tx_info;
use core::test::test_utils::assert_ne;

fn foo(v: felt252) {
    if v == 1 {
        let _tx_info = get_tx_info().unbox();
        assert_ne(@v, @'EXECUTE_AFTER_UPGRADE', 'argent/forbidden-call');
    } else {
        core::internal::revoke_ap_tracking();
    }
}
