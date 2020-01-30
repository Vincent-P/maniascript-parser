use std::error::Error;

use log::{info, trace, warn};
use lsp_server::{ErrorCode, Message, Notification, Request, RequestId, Response};
use lsp_types::{notification::{Notification as NotificationTrait, *}, request::{Request as RequestTrait, *}, *};

pub use crate::app::App;

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

impl App {
    pub fn reply(&mut self, response: Response) {
        trace!("Sending response: {:#?}", response);
        self.conn.sender.send(Message::Response(response)).unwrap();
    }

    pub fn notify(&mut self, notification: Notification) {
        trace!("Sending notification: {:#?}", notification);
        self.conn
            .sender
            .send(Message::Notification(notification))
            .unwrap();
    }

    pub fn err<E>(&mut self, id: RequestId, err: E)
    where
        E: std::fmt::Display,
    {
        warn!("{}", err);
        self.reply(Response::new_err(
            id,
            ErrorCode::UnknownErrorCode as i32,
            err.to_string(),
        ));
    }

    pub fn main_loop(
        &mut self,
        params: serde_json::Value,
    ) -> Result<(), Box<dyn Error + Sync + Send>> {
        let _params: InitializeParams = serde_json::from_value(params).unwrap();
        info!("starting main loop");

        let receiver = self.conn.receiver.clone();
        for msg in &receiver {
            match msg {
                Message::Request(req) => {
                    if self.conn.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    match req.method.as_str() {
                        GotoDefinition::METHOD => {
                            let (id, params) = cast::<GotoDefinition>(req).unwrap();
                            self.go_to_definition(id, params);
                        }

                        DocumentSymbolRequest::METHOD => {
                            let (id, params) = cast::<DocumentSymbolRequest>(req).unwrap();
                            self.document_symbol(id, params);
                        }

                        FoldingRangeRequest::METHOD => {
                            let (id, params) = cast::<FoldingRangeRequest>(req).unwrap();
                            self.folding_ranges(id, params);
                        }

                        _ => (),
                    }
                }

                Message::Notification(not) => match not.method.as_str() {
                    DidOpenTextDocument::METHOD => {
                        let params: DidOpenTextDocumentParams = serde_json::from_value(not.params)?;
                        self.did_open_text_document(params);
                    }

                    DidChangeTextDocument::METHOD => {
                        let params: DidChangeTextDocumentParams =
                            serde_json::from_value(not.params)?;
                        self.did_change_text_document(params);
                    }

                    _ => (),
                },

                Message::Response(_) => {}
            }
        }

        Ok(())
    }
}
