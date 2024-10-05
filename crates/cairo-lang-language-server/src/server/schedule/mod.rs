use std::num::NonZeroUsize;

use anyhow::Result;

pub(crate) mod task;
pub(crate) mod thread;

use self::task::{BackgroundTaskBuilder, SyncTask};
use self::thread::ThreadPriority;
use super::client::Client;
use crate::server::connection::ClientSender;
use crate::server::schedule::task::{BackgroundSchedule, Task};
use crate::state::State;

/// The event loop thread is actually a secondary thread that we spawn from the
/// _actual_ main thread. This secondary thread has a larger stack size
/// than some OS defaults (Windows, for example) and is also designated as
/// high-priority.
pub fn event_loop_thread(
    func: impl FnOnce() -> Result<()> + Send + 'static,
) -> Result<thread::JoinHandle<Result<()>>> {
    // Override OS defaults to avoid stack overflows on platforms with low stack size defaults.
    const MAIN_THREAD_STACK_SIZE: usize = 2 * 1024 * 1024;
    const MAIN_THREAD_NAME: &str = "cairols:main";
    Ok(thread::Builder::new(ThreadPriority::LatencySensitive)
        .name(MAIN_THREAD_NAME.into())
        .stack_size(MAIN_THREAD_STACK_SIZE)
        .spawn(func)?)
}

pub struct Scheduler<'s> {
    state: &'s mut State,
    client: Client<'s>,
    background_pool: thread::Pool,
}

impl<'s> Scheduler<'s> {
    pub fn new(state: &'s mut State, worker_threads: NonZeroUsize, sender: ClientSender) -> Self {
        Self {
            state,
            background_pool: thread::Pool::new(worker_threads),
            client: Client::new(sender),
        }
    }

    /// Immediately sends a request of kind `R` to the client, with associated parameters.
    /// The task provided by `response_handler` will be dispatched as soon as the response
    /// comes back from the client.
    pub fn request<R>(
        &mut self,
        params: R::Params,
        response_handler: impl Fn(R::Result) -> Task<'s> + 'static,
    ) -> Result<()>
    where
        R: lsp_types::request::Request,
    {
        self.client.requester.request::<R>(params, response_handler)
    }

    /// Creates a task to handle a response from the client.
    pub fn response(&mut self, response: lsp_server::Response) -> Task<'s> {
        self.client.requester.pop_response_task(response)
    }

    /// Dispatches a `task` by either running it as a blocking function or
    /// executing it on a background thread pool.
    pub fn dispatch(&mut self, task: Task<'s>) {
        match task {
            Task::Sync(SyncTask { func }) => {
                let notifier = self.client.notifier();
                let responder = self.client.responder();
                func(self.state, notifier, &mut self.client.requester, responder);
            }
            Task::Background(BackgroundTaskBuilder { schedule, builder: func }) => {
                let static_func = func(self.state);
                let notifier = self.client.notifier();
                let responder = self.client.responder();
                let task = move || static_func(notifier, responder);
                match schedule {
                    BackgroundSchedule::Worker => {
                        self.background_pool.spawn(ThreadPriority::Worker, task);
                    }
                    BackgroundSchedule::LatencySensitive => {
                        self.background_pool.spawn(ThreadPriority::LatencySensitive, task)
                    }
                }
            }
        }
    }
}
