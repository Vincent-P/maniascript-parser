use crate::app_state::AppCtx;
use log::info;
use lsp_types::{notification::ShowMessage, InitializeParams, InitializeResult, InitializedParams, MessageType, ShowMessageParams};
use tokio::prelude::{future::ok, Future};

pub fn initialize_handler(
    _param: InitializeParams,
    app: AppCtx,
) -> impl Future<Item = InitializeResult, Error = ()> {
    use lsp_types::{TextDocumentSyncCapability, TextDocumentSyncKind};

    // get the write lock, write() is blocking
    let initialized = app.state.is_initialized.read().unwrap();
    if *initialized {
        info!("Received an initialize request more than once!");
    }

    let mut result = InitializeResult::default();

    // for now we take the entire content of a file
    result.capabilities.text_document_sync =
        Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::Full));

    ok(result)
}

pub fn initialized_handler(_param: InitializedParams, app: AppCtx) {
    // get the write lock, write() is blocking
    let mut initialized = app.state.is_initialized.write().unwrap();
    *initialized = true;

    // send a test message to the client
    let msg = ShowMessageParams {
        typ: MessageType::Info,
        message: "Server correctly initialized!".to_string(),
    };

    app.send_notification::<ShowMessage>(msg);
}
