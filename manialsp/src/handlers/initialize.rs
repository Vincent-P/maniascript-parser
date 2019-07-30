use crate::app_state::AppStateMeta;
use log::info;
use lsp_types::{InitializeParams, InitializeResult, InitializedParams};

pub fn initialize_handler(_param: InitializeParams, _app: AppStateMeta) -> InitializeResult {
    // get the write lock, write() is blocking
    let initialized = _app.is_initialized.read().unwrap();
    if *initialized {
        info!("Received an initialize request more than once!");
    }
    InitializeResult::default()
}

pub fn initialized_handler(_param: InitializedParams, _app: AppStateMeta) {
    // get the write lock, write() is blocking
    let mut initialized = _app.is_initialized.write().unwrap();
    *initialized = true;
}
