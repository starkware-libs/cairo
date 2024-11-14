// +---------------------------------------------------------------+
// | Code adopted from:                                            |
// | Repository: https://github.com/astral-sh/ruff                 |
// | File: `crates/ruff_server/src/server/schedule/thread/pool.rs` |
// | Commit: 46a457318d8d259376a2b458b3f814b9b795fe69              |
// +---------------------------------------------------------------+

// =================================================================

// +------------------------------------------------------------+
// | Code adopted from:                                         |
// | Repository: https://github.com/rust-lang/rust-analyzer.git |
// | File: `crates/stdx/src/thread/pool.rs`                     |
// | Commit: 03b3cb6be9f21c082f4206b35c7fe7f291c94eaa           |
// +------------------------------------------------------------+
//! [`Pool`] implements a basic custom thread pool
//! inspired by the [`threadpool` crate](http://docs.rs/threadpool).
//! When you spawn a task, you specify a thread priority
//! so the pool can schedule it to run on a thread with that priority.
//! rust-analyzer uses this to prioritize work based on latency requirements.
//!
//! The thread pool is implemented entirely using
//! the threading utilities in [`crate::server::schedule::thread`].

use std::cmp::min;
use std::num::NonZero;
use std::thread::available_parallelism;

use crossbeam::channel::{Receiver, Sender, bounded};

use super::{Builder, JoinHandle, ThreadPriority};

pub struct Pool {
    // `_handles` is never read: the field is present
    // only for its `Drop` impl.

    // The worker threads exit once the channel closes;
    // make sure to keep `job_sender` above `handles`
    // so that the channel is actually closed
    // before we join the worker threads!
    job_sender: Sender<Job>,
    _handles: Vec<JoinHandle>,

    parallelism: NonZero<usize>,
}

struct Job {
    requested_priority: ThreadPriority,
    f: Box<dyn FnOnce() + Send + 'static>,
}

impl Pool {
    pub fn new() -> Pool {
        /// Custom stack size, larger than OS defaults, to avoid stack overflows on platforms with
        /// low stack size defaults.
        const STACK_SIZE: usize = 2 * 1024 * 1024;

        const INITIAL_PRIORITY: ThreadPriority = ThreadPriority::Worker;

        /// The default number of threads in the pool in case system parallelism is not available.
        ///
        /// According to docs, [`available_parallelism`] (almost) only fails when the process is
        /// running with limited permissions.
        /// We are making an assumption here that nowadays it is more probable to run without
        /// necessary permissions on a multicore machine than on a single-core one.
        const DEFAULT_PARALLELISM: usize = 4;

        let threads = available_parallelism().map(usize::from).unwrap_or(DEFAULT_PARALLELISM);

        // Channel buffer capacity is between 2 and 4, depending on the pool size.
        let (job_sender, job_receiver) = bounded(min(threads * 2, DEFAULT_PARALLELISM));

        let mut handles = Vec::with_capacity(threads);
        for i in 0..threads {
            let handle = Builder::new(INITIAL_PRIORITY)
                .stack_size(STACK_SIZE)
                .name(format!("cairo-ls:worker:{i}"))
                .spawn({
                    let job_receiver: Receiver<Job> = job_receiver.clone();
                    move || {
                        let mut current_priority = INITIAL_PRIORITY;
                        for job in job_receiver {
                            if job.requested_priority != current_priority {
                                job.requested_priority.apply_to_current_thread();
                                current_priority = job.requested_priority;
                            }
                            (job.f)();
                        }
                    }
                })
                .expect("failed to spawn thread");

            handles.push(handle);
        }

        Pool { _handles: handles, job_sender, parallelism: NonZero::new(threads).unwrap() }
    }

    pub fn spawn<F>(&self, priority: ThreadPriority, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        let f = Box::new(move || {
            if cfg!(debug_assertions) {
                priority.assert_is_used_on_current_thread();
            }
            f();
        });

        let job = Job { requested_priority: priority, f };
        self.job_sender.send(job).unwrap();
    }

    /// Returns a number of tasks that this pool can run concurrently.
    pub fn parallelism(&self) -> NonZero<usize> {
        self.parallelism
    }
}
