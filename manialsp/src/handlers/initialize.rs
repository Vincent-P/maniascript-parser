use crate::app_state::AppCtx;
use log::info;
use lsp_types::{notification::{Notification, ShowMessage, LogMessage}, InitializeParams, InitializeResult, InitializedParams, MessageType, ShowMessageParams};
use tokio::prelude::{future::ok, Future, Sink};

pub fn initialize_handler(
    _param: InitializeParams,
    app: AppCtx,
) -> impl Future<Item = InitializeResult, Error = ()> {
    // get the write lock, write() is blocking
    let initialized = app.state.is_initialized.read().unwrap();
    if *initialized {
        info!("Received an initialize request more than once!");
    }

    ok(InitializeResult::default())
}

pub fn initialized_handler(
    _param: InitializedParams,
    app: AppCtx,
) -> impl Future<Item = (), Error = ()> {
    // get the write lock, write() is blocking
    let mut initialized = app.state.is_initialized.write().unwrap();
    *initialized = true;

    // send a test message to the client
    let msg = ShowMessageParams {
        typ: MessageType::Info,
        message: "Server correctly initialized!".to_string(),
    };

    let r = format!(
        r#"{{"jsonrpc":"2.0","method":"{}","params":{}}}"#,
        ShowMessage::METHOD,
        serde_json::to_value(&msg).unwrap()
    );

    let r2 = format!(
        r#"{{"jsonrpc":"2.0","method":"{}","params":{}}}"#,
        LogMessage::METHOD,
        serde_json::to_value(&msg).unwrap()
    );

    app.sender
        .send(r)
        .and_then(|sender| sender.send(r2))
        .map_err(|_| ())
        .and_then(|_| Ok(()))
}
