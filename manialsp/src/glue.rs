use lib_maniascript::{document::DocumentError, parser::{ParseError}};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use std::convert::TryInto;

pub fn char_range_to_range(span: (usize, usize), text: &str) -> Range {
    let mut line: usize = 0;
    let mut col: usize = 0;
    let mut cur_char: usize = 0;
    let mut start_col: usize = 0;
    let mut start_line: usize = 0;
    let mut end_col: usize = 0;
    let mut end_line: usize = 0;

    for c in text.chars() {
        if cur_char >= span.0 && start_col == 0 && start_line == 0 {
            start_col = col;
            start_line = line;
        }

        if cur_char >= span.1 {
            end_col = col;
            end_line = line;
            break;
        }

        cur_char += c.len_utf8();
        col += c.len_utf8();
        if c == '\n' {
            line += 1;
            col = 0;
        }
    }

    let s = Position::new(
        start_line.try_into().unwrap(),
        start_col.try_into().unwrap(),
    );
    let e = Position::new(end_line.try_into().unwrap(), end_col.try_into().unwrap());
    Range::new(s, e)
}

pub fn parse_error_to_diagnostic(error: &ParseError) -> Diagnostic {
    let (span, msg) = match error {
        ParseError::Token(ref t, Some(e), span) => {
            (span, format!("Expecting a {:?} but got a {:?}", e, t.kind))
        }
        ParseError::String(ref t, Some(ref e), span) => {
            (span, format!("Expecting a {:?} but got a {:?}", e, t.kind))
        }
        ParseError::Token(ref t, None, span) => (span, format!("Got a {:?}", t.kind)),
        ParseError::String(ref t, None, span) => (span, format!("Got a {:?}", t.kind)),
        ParseError::EOF(span) => (span, "Expecting got EOF".to_string()),
    };

    let pos = Position::new(
        (span.line() - 1).try_into().unwrap(),
        span.col().try_into().unwrap(),
    );
    let range = Range::new(pos, pos);
    Diagnostic::new_simple(range, msg)
}

pub fn document_error_to_diagnostic(error: &DocumentError, text: &str) -> Diagnostic {
    match error {
        DocumentError::ValidationError(e) => {
            Diagnostic::new(char_range_to_range(e.span, text),
                            Some(DiagnosticSeverity::Error),
                            None,
                            None,
                            e.msg.to_string(),
                            None)
        }
        DocumentError::ParseError(e) => parse_error_to_diagnostic(e),
    }
}
