use jsonrpc_core::Metadata;
use lsp_types::notification::Notification;
use std::sync::{Arc, RwLock};
use tokio::{prelude::{Future, Sink}, sync::mpsc::Sender};

pub struct AppState {
    pub is_initialized: RwLock<bool>,
}

#[derive(Clone)]
pub struct AppCtx {
    pub state: Arc<AppState>,
    pub sender: Sender<String>,
}

impl AppState {
    pub fn new() -> Self {
        AppState {
            is_initialized: RwLock::new(false),
        }
    }
}

impl AppCtx {
    pub fn new(state: AppState, sender: Sender<String>) -> Self {
        AppCtx {
            state: Arc::new(state),
            sender,
        }
    }

    pub fn send_notification<N>(&self, params: N::Params) -> impl Future<Item = (), Error = ()>
    where
        N: Notification,
        N::Params: serde::Serialize,
    {
        let n = format!(
            r#"{{"jsonrpc":"2.0","method":"{}","params":{}}}"#,
            N::METHOD,
            serde_json::to_value(params).unwrap()
        );

        let sender = self.sender.clone();

        sender.send(n).map_err(|_| ()).and_then(|_| Ok(()))
    }
}

impl Metadata for AppCtx {}
