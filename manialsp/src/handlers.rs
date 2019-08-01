pub mod initialize;
use crate::app_state::AppCtx;
pub use initialize::*;
use jsonrpc_core::{types::params::Params, MetaIoHandler};
use log::info;
use lsp_types::{notification::{Initialized, Notification}, request::{Initialize, Request}};
use tokio::prelude::{future::IntoFuture, Future};

trait LspHandler {
    fn add_request_handler<R, F, A, B>(&mut self, handler: F)
    where
        R: Request,
        for<'de> R::Params: serde::Serialize + serde::de::Deserialize<'de> + std::fmt::Debug,
        R::Result: serde::Serialize + serde::de::DeserializeOwned,
        A: Future<Item = R::Result, Error = ()> + Send + 'static,
        B: IntoFuture<Item = R::Result, Error = (), Future = A>,
        F: 'static + Send + Sync + Fn(R::Params, AppCtx) -> B;

    fn add_notification_handler<N, F, A, B>(&mut self, handler: F)
    where
        N: Notification,
        for<'de> N::Params: serde::Serialize + serde::de::Deserialize<'de> + std::fmt::Debug,
        A: Future<Item = (), Error = ()> + Send + 'static,
        B: IntoFuture<Item = (), Error = (), Future = A>,
        F: 'static + Send + Sync + Fn(N::Params, AppCtx) -> B;
}

impl LspHandler for MetaIoHandler<AppCtx> {
    fn add_request_handler<R, F, A, B>(&mut self, handler: F)
    where
        R: Request,
        for<'de> R::Params: serde::Serialize + serde::de::Deserialize<'de> + std::fmt::Debug,
        R::Result: serde::Serialize + serde::de::DeserializeOwned,
        A: Future<Item = R::Result, Error = ()> + Send + 'static,
        B: IntoFuture<Item = R::Result, Error = (), Future = A>,
        F: 'static + Send + Sync + Fn(R::Params, AppCtx) -> B,
    {
        info!("Adding request handler for {}", R::METHOD);
        self.add_method_with_meta(R::METHOD, move |params: Params, app: AppCtx| {
            let params: R::Params = params.parse().unwrap();

            info!("Request param: {:?}", params);

            handler(params, app)
                .into_future()
                .and_then(|value| {
                    let value = serde_json::to_value(value).unwrap();
                    Ok(value)
                })
                .map_err(|e| panic!("{:?}", e))
        });
    }

    fn add_notification_handler<N, F, A, B>(&mut self, handler: F)
    where
        N: Notification,
        for<'de> N::Params: serde::Serialize + serde::de::Deserialize<'de> + std::fmt::Debug,
        A: Future<Item = (), Error = ()> + Send + 'static,
        B: IntoFuture<Item = (), Error = (), Future = A>,
        F: 'static + Send + Sync + Fn(N::Params, AppCtx) -> B,
    {
        info!("Adding notification handler for {}", N::METHOD);
        self.add_notification_with_meta(N::METHOD, move |params: Params, app: AppCtx| {
            let params: N::Params = params.parse().unwrap();
            info!("Notification param: {:?}", params);
            tokio::spawn(handler(params, app).into_future());
        });
    }
}

pub fn register_handlers(io: &mut MetaIoHandler<AppCtx>) {
    io.add_request_handler::<Initialize, _, _, _>(initialize::initialize_handler);
    io.add_notification_handler::<Initialized, _, _, _>(initialize::initialized_handler);
}
