use crate::app_state::AppCtx;
use log::info;
use lsp_types::{notification::ShowMessage, MessageType, ShowMessageParams};
use tokio::prelude::{future::ok, Future};

pub fn shutdown_handler(_param: (), app: AppCtx) -> impl Future<Item = (), Error = ()> {
    let mut has_shutdown = app.state.has_shutdown.write().unwrap();
    *has_shutdown = true;

    info!("Received shutdown!");

    // send a test message to the client
    let msg = ShowMessageParams {
        typ: MessageType::Info,
        message: "Server correctly shutdown!".to_string(),
    };

    app.send_notification_future::<ShowMessage>(msg).map(|_| ())
}

pub fn exit_handler(_param: (), app: AppCtx) {
    let has_shutdown = app.state.has_shutdown.read().unwrap();
    info!("Exiting...!");
    ::std::process::exit(match *has_shutdown {
        true => 0,
        false => 1,
    });
}
