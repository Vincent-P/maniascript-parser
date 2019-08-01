use jsonrpc_core::Metadata;
use std::sync::{Arc, RwLock};
use tokio::sync::mpsc::Sender;

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
}

impl Metadata for AppCtx {}
