// +---------------------------------------------------+
// | Code adopted from:                                |
// | Repository: https://github.com/astral-sh/ruff     |
// | File: `crates/ruff_server/src/server/schedule.rs` |
// | Commit: 46a457318d8d259376a2b458b3f814b9b795fe69  |
// +---------------------------------------------------+

use anyhow::Result;
use task::BackgroundTaskBuilder;

use crate::server::client::{Client, Notifier, Requester, Responder};
use crate::server::connection::ClientSender;
use crate::state::State;
use crate::thread::ThreadPriority;

pub(crate) mod task;
pub(crate) mod thread;

pub(super) use task::BackgroundSchedule;
pub(crate) use task::{SyncTask, Task};
pub(crate) use thread::JoinHandle;

/// The event loop thread is actually a secondary thread that we spawn from the
/// _actual_ main thread. This secondary thread has a larger stack size
/// than some OS defaults (Windows, for example) and is also designated as
/// high priority.
pub fn event_loop_thread(
    func: impl FnOnce() -> Result<()> + Send + 'static,
) -> Result<JoinHandle<Result<()>>> {
    // Override OS defaults to avoid stack overflows on platforms with low stack size defaults.
    const MAIN_THREAD_STACK_SIZE: usize = 2 * 1024 * 1024;
    const MAIN_THREAD_NAME: &str = "cairols:main";
    Ok(thread::Builder::new(ThreadPriority::LatencySensitive)
        .name(MAIN_THREAD_NAME.into())
        .stack_size(MAIN_THREAD_STACK_SIZE)
        .spawn(func)?)
}

pub type PostHook = dyn Fn(&mut State, Notifier);

pub struct Scheduler<'s> {
    state: &'s mut State,
    client: Client<'s>,
    background_pool: thread::SharedPool,
    sync_task_post_hooks: Vec<Box<PostHook>>,
}

impl<'s> Scheduler<'s> {
    pub fn new(
        state: &'s mut State,
        background_pool: thread::SharedPool,
        sender: ClientSender,
    ) -> Self {
        Self {
            state,
            background_pool,
            client: Client::new(sender),
            sync_task_post_hooks: Default::default(),
        }
    }

    // Executes after every local `task`. Meaning this will happen after every possible state
    // change.
    #[inline]
    pub(crate) fn sync_task_post_hook(&mut self, func: impl Fn(&mut State, Notifier) + 'static) {
        self.sync_task_post_hooks.push(Box::new(func));
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
                func(self.state, notifier.clone(), &mut self.client.requester, responder);

                for hook in &self.sync_task_post_hooks {
                    hook(self.state, notifier.clone());
                }
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

    // Dispatches a local `task`.
    pub(crate) fn local(
        &mut self,
        func: impl FnOnce(&mut State, Notifier, &mut Requester<'_>, Responder) + 's,
    ) {
        self.dispatch(Task::local(func));
    }

    // Dispatches a background `task`.
    pub(crate) fn background(
        &mut self,
        schedule: BackgroundSchedule,
        func: impl FnOnce(Notifier, Responder) + Send + 'static,
    ) {
        self.dispatch(Task::background(schedule, |_state| Box::new(func)));
    }
}
