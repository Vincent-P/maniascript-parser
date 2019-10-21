use jsonrpc_core::Metadata;
use lib_maniascript::document::Document;
use lsp_types::{notification::Notification, Url};
use std::{collections::HashMap, sync::{Arc, RwLock}};
use tokio::{prelude::{Future, Sink}, sync::mpsc::Sender};

pub struct AppState {
    pub is_initialized: RwLock<bool>,
    pub has_shutdown: RwLock<bool>,
    pub documents: RwLock<HashMap<Url, Document>>,
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
            has_shutdown: RwLock::new(false),
            documents: RwLock::new(HashMap::new()),
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

    pub fn send_notification_future<N>(
        &self,
        params: N::Params,
    ) -> impl Future<Item = (), Error = ()>
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

    pub fn send_notification<N>(&self, params: N::Params)
    where
        N: Notification + 'static,
        N::Params: serde::Serialize,
    {
        tokio::spawn(self.send_notification_future::<N>(params));
    }
}

impl Metadata for AppCtx {}
