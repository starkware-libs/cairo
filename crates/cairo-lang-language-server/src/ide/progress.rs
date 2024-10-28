use crate::server::client::Notifier;
use lsp_types::notification::Notification;
use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub enum Progress {
    Begin,
    End
}

#[derive(Deserialize, Serialize)]
struct ProgressBeginParams {
    title: String
}

#[derive(Deserialize, Serialize)]
enum ProgressStageParams { 
    ProgressBegin(ProgressBeginParams),
    ProgressEnd
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
struct ProgressParams { 
    id: String,
    value: ProgressStageParams,
}

struct ProgressNotification;

impl Notification for ProgressNotification {
    type Params = ProgressParams;
    const METHOD: &'static str = "cairo/progress";
}

pub(crate) struct ProgressReporter<'a> { 
   notifier: &'a Notifier
}

impl<'a> ProgressReporter<'a> { 
    pub(crate) fn new(notifier: &'a Notifier) -> ProgressReporter<'a> {
        Self { 
            notifier
        }
    }
    
    /// Sends a custom notification to the client to report work in progress
    pub(crate) fn report_progress(
        &mut self,
        id: &str, 
        title: &str,
        state: Progress,
    ) {
        tracing::debug!(?title, ?state);
        let progress_stage_params = match state {
            Progress::Begin => {
                ProgressStageParams::ProgressBegin(ProgressBeginParams {
                    title: title.into(),
                })
            }
            Progress::End => {
                ProgressStageParams::ProgressEnd
            }
        };
        self.notifier.notify::<ProgressNotification>(ProgressParams { 
            id: id.to_string(),
            value: progress_stage_params
        });
    }
}