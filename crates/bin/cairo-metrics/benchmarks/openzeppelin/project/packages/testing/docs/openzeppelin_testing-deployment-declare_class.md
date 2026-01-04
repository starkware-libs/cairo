# declare_class

Declares a contract with a `snforge_std::declare` call and unwraps the result. This function will skip declaration and just return the `ContractClass` if the contract is already declared (the result of `snforge_std::declare` call is of type `DeclareResult::AlreadyDeclared`)

Fully qualified path: `openzeppelin_testing::deployment::declare_class`

<pre><code class="language-rust">pub fn declare_class(contract_name: ByteArray) -&gt; ContractClass</code></pre>

