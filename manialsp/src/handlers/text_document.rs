use crate::{app_state::AppCtx, glue::*};
use lib_maniascript::document::*;
use log::info;
use lsp_types::{notification::PublishDiagnostics, DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, PublishDiagnosticsParams, Url};

pub fn did_save_handler(_param: DidSaveTextDocumentParams, _app: AppCtx) {}

fn validate_document(app: AppCtx, uri: Url, text: &str) {
    info!("Validating document {}", uri.as_str());

    let mut documents = app.state.documents.write().unwrap();

    if !documents.contains_key(&uri) {
        let file_path = uri.to_file_path().unwrap();
        documents.insert(uri.clone(), Document::new(file_path, text));
    }

    let document = documents.get_mut(&uri).unwrap();

    document.update(text);

    let diagnostics = document
        .errors()
        .iter()
        .map(|e| document_error_to_diagnostic(e, text))
        .collect();

    let params = PublishDiagnosticsParams::new(uri, diagnostics);
    app.send_notification::<PublishDiagnostics>(params);
}

pub fn did_open_handler(_param: DidOpenTextDocumentParams, app: AppCtx) {
    let uri = _param.text_document.uri;
    let text = &_param.text_document.text;
    validate_document(app, uri, text);
}

pub fn did_change_handler(_param: DidChangeTextDocumentParams, app: AppCtx) {
    let uri = _param.text_document.uri;
    let text = &_param.content_changes[0].text;
    validate_document(app, uri, text);
}
