# declare_and_deploy_at

Combines the declaration of a class and the deployment of a contract at the given address into one function call. This function will skip declaration if the contract is already declared (the result of `snforge_std::declare` call is of type `DeclareResult::AlreadyDeclared`)

Fully qualified path: `openzeppelin_testing::deployment::declare_and_deploy_at`

<pre><code class="language-rust">pub fn declare_and_deploy_at(
    contract_name: ByteArray, target_address: ContractAddress, calldata: Array&lt;felt252&gt;,
)</code></pre>

