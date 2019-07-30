pub mod initialize;
use crate::app_state::AppStateMeta;
pub use initialize::*;
use jsonrpc_core::{types::params::Params, MetaIoHandler};
use log::info;
use lsp_types::{notification::{Initialized, Notification}, request::{Initialize, Request}};

trait LspHandler {
    fn add_request_handler<R, F>(&mut self, handler: F)
    where
        R: Request,
        for<'de> R::Params: serde::Serialize + serde::de::Deserialize<'de> + std::fmt::Debug,
        R::Result: serde::Serialize + serde::de::DeserializeOwned,
        F: 'static + Send + Sync + Fn(R::Params, AppStateMeta) -> R::Result;

    fn add_notification_handler<N, F>(&mut self, handler: F)
    where
        N: Notification,
        for<'de> N::Params: serde::Serialize + serde::de::Deserialize<'de> + std::fmt::Debug,
        F: 'static + Send + Sync + Fn(N::Params, AppStateMeta);
}

impl LspHandler for MetaIoHandler<AppStateMeta> {
    fn add_request_handler<R, F>(&mut self, handler: F)
    where
        R: Request,
        for<'de> R::Params: serde::Serialize + serde::de::Deserialize<'de> + std::fmt::Debug,
        R::Result: serde::Serialize + serde::de::DeserializeOwned,
        F: 'static + Send + Sync + Fn(R::Params, AppStateMeta) -> R::Result,
    {
        self.add_method_with_meta(R::METHOD, move |params: Params, app: AppStateMeta| {
            let param: R::Params = params.parse().unwrap();

            info!("Request param: {:?}", param);

            let value = handler(param, app);
            let value = serde_json::to_value(value).unwrap();

            Ok(value)
        });
    }

    fn add_notification_handler<N, F>(&mut self, handler: F)
    where
        N: Notification,
        for<'de> N::Params: serde::Serialize + serde::de::Deserialize<'de> + std::fmt::Debug,
        F: 'static + Send + Sync + Fn(N::Params, AppStateMeta),
    {
        self.add_notification_with_meta(N::METHOD, move |params: Params, app: AppStateMeta| {
            let param: N::Params = params.parse().unwrap();
            info!("Notification param: {:?}", param);
            handler(param, app);
        });
    }
}

pub fn register_handlers(io: &mut MetaIoHandler<AppStateMeta>) {
    io.add_request_handler::<Initialize, _>(initialize::initialize_handler);
    io.add_notification_handler::<Initialized, _>(initialize::initialized_handler);
}
