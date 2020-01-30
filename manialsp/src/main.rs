use std::error::Error;

use log::info;
use lsp_server::Connection;
use lsp_types::*;

mod app;
use crate::app::App;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    flexi_logger::Logger::with_str("info").start().unwrap();
    info!("Starting ManiaLSP");

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::Full),
                ..TextDocumentSyncOptions::default()
            },
        )),
        document_symbol_provider: Some(true),
        definition_provider: Some(true),
        folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
        ..ServerCapabilities::default()
    })
    .unwrap();

    let initialization_params = connection.initialize(server_capabilities)?;

    info!("initialization params: {:?}", initialization_params);

    App::new(connection).main_loop(initialization_params)?;

    io_threads.join()?;

    info!("shutting down server");
    Ok(())
}
