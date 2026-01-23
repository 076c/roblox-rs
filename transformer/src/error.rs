use proc_macro2::Span;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl Diagnostic {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        let start = span.unwrap();
        Self {
            message: message.into(),
            line: start.line(),
            column: start.column(),
        }
    }
}

pub type DiagResult<T> = Result<T, Diagnostic>;
