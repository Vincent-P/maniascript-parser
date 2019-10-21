use crate::{app_state::{AppCtx, AppState}, codecs::LspRequestCodec};
use jsonrpc_core::MetaIoHandler;
use std::sync::Arc;
use tokio::{self, prelude::{future::lazy, Future, Sink, Stream}, sync::mpsc};

use tokio_codec::{FramedRead, FramedWrite};

pub struct ServerBuilder {
    handler: Arc<MetaIoHandler<AppCtx>>,
}

impl ServerBuilder {
    pub fn new<T>(handler: T) -> Self
    where
        T: Into<MetaIoHandler<AppCtx>>,
    {
        ServerBuilder {
            handler: Arc::new(handler.into()),
        }
    }

    /// Will block until EOF is read or until an error occurs.
    /// The server reads from STDIN request-by-request
    /// and each response is written to STDOUT.
    pub fn build(&self, app: AppState) {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let stdin = FramedRead::new(stdin, LspRequestCodec::new());
        let stdout = FramedWrite::new(stdout, LspRequestCodec::new());

        let handler = Arc::clone(&self.handler);

        tokio::run(lazy(|| {
            let (tx, rx) = mpsc::channel(1_024);
            let app_ctx = AppCtx::new(app, tx.clone());

            tokio::spawn(lazy(move || {
                stdin
                    .and_then(move |request| {
                        handler
                            .handle_request(&request, app_ctx.clone())
                            .map(|response| match response {
                                Some(res) => res,
                                None => String::from(""),
                            })
                            .map_err(|e| panic!("{:?}", e))
                    })
                    .map_err(|e| panic!("{:?}", e))
                    .fold(tx.clone(), |out, response| {
                        out.send(response).map_err(|_| ())
                    })
                    .map(|_| ())
                    .map_err(|e| panic!("{:?}", e))
            }));

            rx.map_err(|_| ())
                .fold(stdout, |out, response| out.send(response).map_err(|_| ()))
                .map(|_| ())
        }));
    }
}
