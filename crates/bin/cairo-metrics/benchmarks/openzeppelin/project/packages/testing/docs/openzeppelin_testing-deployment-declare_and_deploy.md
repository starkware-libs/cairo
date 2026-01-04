# declare_and_deploy

Combines the declaration of a class and the deployment of a contract into one function call. This function will skip declaration if the contract is already declared (the result of `snforge_std::declare` call is of type `DeclareResult::AlreadyDeclared`)

Fully qualified path: `openzeppelin_testing::deployment::declare_and_deploy`

<pre><code class="language-rust">pub fn declare_and_deploy(contract_name: ByteArray, calldata: Array&lt;felt252&gt;) -&gt; ContractAddress</code></pre>

