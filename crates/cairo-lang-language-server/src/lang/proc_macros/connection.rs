use std::io::{BufRead, BufReader, Write};

use crossbeam::channel::{Receiver, Sender};
use proc_macro_server_api::{RpcRequest, RpcResponse};
use tracing::{error, info};

#[derive(Debug)]
pub struct ProcMacroServerConnection {
    pub(super) requester: Sender<RpcRequest>,
    pub(super) responder: Receiver<RpcResponse>,
}

impl ProcMacroServerConnection {
    pub fn new(mut proc_macro_server: std::process::Child) -> Self {
        let server_output = proc_macro_server.stdout.take().unwrap();
        let mut server_input = proc_macro_server.stdin.take().unwrap();

        let (sender, responder) = crossbeam::channel::unbounded();
        let (requester, receiver) = crossbeam::channel::bounded(0);

        std::thread::spawn(move || {
            let mut line = String::new();
            let mut output = BufReader::new(server_output);

            loop {
                line.clear();

                let Ok(bytes) = output.read_line(&mut line) else {
                    error!("Error occurred while reading from proc-macro-server");
                    break;
                };

                // This mean end of stream.
                if bytes == 0 {
                    // Wait for process to exit as it will not produce any more data.
                    let _ = proc_macro_server.wait();

                    break;
                }

                if line.trim().is_empty() {
                    continue;
                }

                let Ok(response) = serde_json::from_str::<RpcResponse>(&line) else {
                    error!("Error occurred while deserializing response, used input:\n{line}");

                    break;
                };

                if sender.send(response).is_err() {
                    info!("Stopped reading from proc-macro-server");

                    // No receiver exists so stop reading and drop this thread.
                    break;
                }
            }
        });

        std::thread::spawn(move || {
            for request in receiver {
                let mut request = serde_json::to_vec(&request).unwrap();

                request.push(b'\n');

                if server_input.write_all(&request).is_err() {
                    error!("Error occurred while writing to proc-macro-server");

                    break;
                }
            }
        });

        Self { requester, responder }
    }
}
