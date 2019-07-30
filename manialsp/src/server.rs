use crate::{app_state::AppStateMeta, codecs::LspRequestCodec};
use jsonrpc_core::MetaIoHandler;
use log::info;
use std::sync::Arc;
use tokio::{self, prelude::{Future, Sink, Stream}};
use tokio_codec::{FramedRead, FramedWrite};

pub struct ServerBuilder {
    handler: Arc<MetaIoHandler<AppStateMeta>>,
}

impl ServerBuilder {
    pub fn new<T>(handler: T) -> Self
    where
        T: Into<MetaIoHandler<AppStateMeta>>,
    {
        ServerBuilder {
            handler: Arc::new(handler.into()),
        }
    }

    /// Will block until EOF is read or until an error occurs.
    /// The server reads from STDIN request-by-request
    /// and each response is written to STDOUT.
    pub fn build(&self, app: AppStateMeta) {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let stdin = FramedRead::new(stdin, LspRequestCodec::new());
        let stdout = FramedWrite::new(stdout, LspRequestCodec::new());

        let handler = Arc::clone(&self.handler);
        let app = Arc::clone(&app);

        let future = stdin
            .and_then(move |request| process(&handler, &app, request).map_err(|_| unreachable!()))
            .fold(stdout, |out, response| {
                out.send(response)
            })
            .map(|_| ())
            .map_err(|e| panic!("{:?}", e));

        tokio::run(future);
    }
}

/// Process a request asynchronously
fn process(
    io: &Arc<MetaIoHandler<AppStateMeta>>,
    app: &AppStateMeta,
    input: String,
) -> impl Future<Item = String, Error = ()> + Send {
    info!("Processing jsonrpc request: {}", &input);

    io.handle_request(&input, Arc::clone(app))
        .map(move |result| match result {
            Some(res) => {
                info!("Sending response {}", &res);
                res
            }
            None => {
                info!("JSON RPC request produced no response: {:?}", input);
                String::from("")
            }
        })
}
