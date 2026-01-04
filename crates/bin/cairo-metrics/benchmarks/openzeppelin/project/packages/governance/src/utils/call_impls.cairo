// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (governance/src/utils/call_impls.cairo)

use core::hash::{Hash, HashStateExTrait, HashStateTrait};
use starknet::account::Call;

pub impl HashCallImpl<S, +HashStateTrait<S>, +Drop<S>> of Hash<Call, S> {
    fn update_state(mut state: S, value: Call) -> S {
        let Call { to, selector, calldata } = value;
        state = state.update_with(to).update_with(selector).update_with(calldata.len());
        for elem in calldata {
            state = state.update_with(*elem);
        }

        state
    }
}

pub impl HashCallsImpl<S, +HashStateTrait<S>, +Drop<S>> of Hash<Span<Call>, S> {
    fn update_state(mut state: S, value: Span<Call>) -> S {
        state = state.update_with(value.len());
        for elem in value {
            state = state.update_with(*elem);
        }
        state
    }
}

pub impl CallPartialEq of PartialEq<Call> {
    #[inline(always)]
    fn eq(lhs: @Call, rhs: @Call) -> bool {
        let Call { to: l_to, selector: l_selector, calldata: l_calldata } = lhs;
        let Call { to: r_to, selector: r_selector, calldata: r_calldata } = rhs;
        l_to == r_to && l_selector == r_selector && l_calldata == r_calldata
    }
    #[inline(always)]
    fn ne(lhs: @Call, rhs: @Call) -> bool {
        lhs != rhs
    }
}
