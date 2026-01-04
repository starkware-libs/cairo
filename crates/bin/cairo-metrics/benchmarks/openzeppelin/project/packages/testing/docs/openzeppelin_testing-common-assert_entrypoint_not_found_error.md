# assert_entrypoint_not_found_error

Asserts that the syscall result of a call failed with an "Entrypoint not found" error, following the Starknet Foundry emitted error format.

Fully qualified path: `openzeppelin_testing::common::assert_entrypoint_not_found_error`

<pre><code class="language-rust">pub fn assert_entrypoint_not_found_error&lt;T, +Drop&lt;T&gt;&gt;(
    result: SyscallResult&lt;T&gt;, selector: felt252, contract_address: ContractAddress,
)</code></pre>

