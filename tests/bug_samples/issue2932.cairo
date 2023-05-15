use starknet::get_tx_info;
use box::BoxTrait;

fn foo(v: felt252) {
    if v == 1 {
        let tx_info = get_tx_info().unbox();
        assert(v != 'EXECUTE_AFTER_UPGRADE', 'argent/forbidden-call');
    } else {
        internal::revoke_ap_tracking();
    }
}
