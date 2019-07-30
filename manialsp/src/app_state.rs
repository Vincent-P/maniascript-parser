use std::sync::{Arc, RwLock};

pub struct AppState {
    pub is_initialized: RwLock<bool>,
}

pub type AppStateMeta = Arc<AppState>;

impl AppState {
    pub fn new() -> Self {
        AppState {
            is_initialized: RwLock::new(false),
        }
    }
}
