use std::collections::VecDeque;
use std::io::{BufRead, BufReader, Write};
use std::sync::{Arc, Mutex};

use crossbeam::channel::{Sender, TrySendError};
use scarb_proc_macro_server_types::jsonrpc::{RpcRequest, RpcResponse};
use tracing::{error, info};

pub struct ProcMacroServerConnection {
    pub(super) requester: Sender<RpcRequest>,
    pub(super) responses: Arc<Mutex<VecDeque<RpcResponse>>>,
}

impl std::fmt::Debug for ProcMacroServerConnection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ProcMacroServerConnection")
            .field("requester", &self.requester)
            .finish_non_exhaustive()
    }
}

impl ProcMacroServerConnection {
    pub fn new(
        mut proc_macro_server: std::process::Child,
        response_channel_sender: Sender<()>,
    ) -> Self {
        let server_output =
            proc_macro_server.stdout.take().expect("proc-macro-server must use pipe on stdout");
        let mut server_input =
            proc_macro_server.stdin.take().expect("proc-macro-server must use pipe on stdin");

        let (requester, receiver) = crossbeam::channel::bounded(0);

        let responses: Arc<Mutex<VecDeque<RpcResponse>>> = Default::default();
        let responses_writer = Arc::clone(&responses);

        std::thread::spawn(move || {
            let mut line = String::new();
            let mut output = BufReader::new(server_output);

            loop {
                line.clear();

                let Ok(bytes) = output.read_line(&mut line) else {
                    error!("error occurred while reading from proc-macro-server");
                    break;
                };

                // This mean end of stream.
                if bytes == 0 {
                    break;
                }

                if line.trim().is_empty() {
                    continue;
                }

                let response = match serde_json::from_str::<RpcResponse>(&line) {
                    Ok(response) => response,
                    Err(err) => {
                        error!("error occurred while deserializing response: {err:?}");
                        break;
                    }
                };

                responses_writer.lock().unwrap().push_back(response);
                if let Err(TrySendError::Disconnected(_)) = response_channel_sender.try_send(()) {
                    info!("stopped reading from proc-macro-server");

                    // No receiver exists so stop reading and drop this thread.
                    break;
                }
            }

            // Make sure server is closed.
            let _ = proc_macro_server.kill();
        });

        std::thread::spawn(move || {
            for request in receiver {
                let mut request = serde_json::to_vec(&request).unwrap();

                request.push(b'\n');

                if let Err(err) = server_input.write_all(&request) {
                    error!("error occurred while writing to proc-macro-server: {err:?}");

                    break;
                }
            }
        });

        Self { requester, responses }
    }
}
