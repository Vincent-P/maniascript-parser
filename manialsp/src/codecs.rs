use std::io;
use std::str;
use tokio_codec::{Decoder, Encoder};
use bytes::BytesMut;

pub struct LspRequestCodec {
    start_index: usize,
    len: Option<usize>
}

impl LspRequestCodec {
    pub fn new() -> LspRequestCodec {
        LspRequestCodec {
            start_index: 0,
            len: None
        }
    }
}

impl Decoder for LspRequestCodec {
    type Item = String;
    type Error = io::Error;

    fn decode(&mut self, buf: &mut BytesMut) -> Result<Option<String>, io::Error> {
        if let Some(content_length) = self.len
        {
            let request_end = self.start_index + content_length;

            // The buffer contains the entire request
            if buf.len() >  request_end {
                let request = buf.split_to(request_end);
                let request = &request[self.start_index..];
                let request = str::from_utf8(&request).expect("invalid utf8 data");

                self.start_index = 0;
                self.len = None;

                Ok(Some(request.to_string()))
            }

            else {
                Ok(None)
            }
        }

        else {
            let request = str::from_utf8(&buf).expect("invalid utf8 data");

            if let Some(content_length) = request.find("Content-Length: ") {
                if let Some(end) = request[content_length..].find("\r\n\r\n") {
                    let header_start = content_length + "Content-Length: ".len();
                    let cl_end = end + content_length;

                    let length = &buf[header_start..cl_end];
                    let length = str::from_utf8(&length).expect("invalid utf8 data");

                    let header_end = cl_end + "\r\n\r\n".len();

                    self.start_index = header_end;

                    match length.parse::<usize>() {
                        Ok(l) => self.len = Some(l),
                        Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidInput, e))
                    }
                }
            }

            Ok(None)
        }
    }
}
