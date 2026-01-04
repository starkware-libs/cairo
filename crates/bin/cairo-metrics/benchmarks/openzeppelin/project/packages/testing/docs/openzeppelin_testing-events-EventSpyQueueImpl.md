# EventSpyQueueImpl

Fully qualified path: `openzeppelin_testing::events::EventSpyQueueImpl`

<pre><code class="language-rust">pub impl EventSpyQueueImpl of EventSpyExt</code></pre>

## Impl functions

### get_events

Fully qualified path: `openzeppelin_testing::events::EventSpyQueueImpl::get_events`

<pre><code class="language-rust">fn get_events(ref self: EventSpyQueue) -&gt; Events</code></pre>


### assert_only_event

Ensures that `from_address` has emitted only the `expected_event` and no additional events.

Fully qualified path: `openzeppelin_testing::events::EventSpyQueueImpl::assert_only_event`

<pre><code class="language-rust">fn assert_only_event&lt;T, +starknet::Event&lt;T&gt;, +Drop&lt;T&gt;&gt;(
    ref self: EventSpyQueue, from_address: ContractAddress, expected_event: T,
)</code></pre>


### assert_emitted_single

Ensures that `from_address` has emitted the `expected_event`. This assertion increments the event offset which essentially consumes the event in the first position of the offset. This means that events must be asserted in the order that they're emitted.

Fully qualified path: `openzeppelin_testing::events::EventSpyQueueImpl::assert_emitted_single`

<pre><code class="language-rust">fn assert_emitted_single&lt;T, +starknet::Event&lt;T&gt;, +Drop&lt;T&gt;&gt;(
    ref self: EventSpyQueue, from_address: ContractAddress, expected_event: T,
)</code></pre>


### drop_event

Removes a single event from the queue. If the queue is empty, the function will panic.

Fully qualified path: `openzeppelin_testing::events::EventSpyQueueImpl::drop_event`

<pre><code class="language-rust">fn drop_event(ref self: EventSpyQueue)</code></pre>


### drop_n_events

Removes `number_to_drop` events from the queue. If the queue is empty, the function will panic.

Fully qualified path: `openzeppelin_testing::events::EventSpyQueueImpl::drop_n_events`

<pre><code class="language-rust">fn drop_n_events(ref self: EventSpyQueue, number_to_drop: u32)</code></pre>


### drop_all_events

Removes all events remaining on the queue. If the queue is empty already, the function will do nothing.

Fully qualified path: `openzeppelin_testing::events::EventSpyQueueImpl::drop_all_events`

<pre><code class="language-rust">fn drop_all_events(ref self: EventSpyQueue)</code></pre>


### assert_no_events_left

Ensures that there are no events remaining on the queue.

Fully qualified path: `openzeppelin_testing::events::EventSpyQueueImpl::assert_no_events_left`

<pre><code class="language-rust">fn assert_no_events_left(ref self: EventSpyQueue)</code></pre>


### assert_no_events_left_from

Ensures that there are no events emitted from the given address remaining on the queue.

Fully qualified path: `openzeppelin_testing::events::EventSpyQueueImpl::assert_no_events_left_from`

<pre><code class="language-rust">fn assert_no_events_left_from(ref self: EventSpyQueue, from_address: ContractAddress)</code></pre>


### count_events_from

Counts the number of remaining events emitted from the given address.

Fully qualified path: `openzeppelin_testing::events::EventSpyQueueImpl::count_events_from`

<pre><code class="language-rust">fn count_events_from(ref self: EventSpyQueue, from_address: ContractAddress) -&gt; u32</code></pre>


