# EventSpyQueue

A wrapper around the `EventSpy` structure to allow treating the events as a queue.

Fully qualified path: `openzeppelin_testing::events::EventSpyQueue`

<pre><code class="language-rust">#[derive(Drop, Serde)]
pub struct EventSpyQueue {
    event_offset: usize,
    event_spy: EventSpy,
}</code></pre>

