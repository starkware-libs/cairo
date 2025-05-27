use std::sync::{Arc, Mutex};

use salsa::Event;

#[salsa::db]
#[derive(Clone)]
pub struct LoggerDb {
    storage: salsa::Storage<Self>,
    #[allow(dead_code)]
    logger: Logger,
}

impl Default for LoggerDb {
    fn default() -> Self {
        let logger = Logger::default();
        let moved_logger = logger.clone();
        let log = Box::new(move |event: Event| match event.kind {
            salsa::EventKind::WillExecute { .. }
            | salsa::EventKind::WillCheckCancellation
            | salsa::EventKind::DidValidateMemoizedValue { .. }
            | salsa::EventKind::WillDiscardStaleOutput { .. }
            | salsa::EventKind::DidDiscard { .. } => {
                moved_logger.logs.lock().unwrap().push(format!("salsa_event({:?})", event.kind));
            }
            _ => {}
        });
        let storage = salsa::Storage::new(Some(log));
        Self { storage, logger }
    }
}

#[derive(Default, Clone)]
struct Logger {
    logs: Arc<Mutex<Vec<String>>>,
}

#[salsa::db]
impl salsa::Database for LoggerDb {}

impl LoggerDb {
    /// Asserts what the (formatted) logs should look like,
    /// clearing the logged events. This takes `&mut self` because
    /// it is meant to be run from outside any tracked functions.
    #[allow(dead_code)]
    pub fn assert_logs(&self, expected: expect_test::Expect) {
        let logs = std::mem::take(&mut *self.logger.logs.lock().unwrap());
        expected.assert_eq(&format!("{logs:#?}"));
    }
}
