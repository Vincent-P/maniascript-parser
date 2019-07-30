mod app_state;
mod codecs;
mod handlers;
mod server;

use app_state::AppState;
use flexi_logger;
use handlers::register_handlers;
use jsonrpc_core::MetaIoHandler;
use log::info;
use server::ServerBuilder;
use std::sync::Arc;

fn main() {
    flexi_logger::Logger::with_str("info")
        .log_to_file()
        .start()
        .unwrap();

    info!("ManiaLSP started.");

    let mut io = MetaIoHandler::default();
    let app = Arc::new(AppState::new());

    register_handlers(&mut io);

    ServerBuilder::new(io).build(app);
}
