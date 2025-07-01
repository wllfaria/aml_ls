use serde::Serialize;
use crate::Location;

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Diagnostic {
    pub location: Location,
    pub severity: Severity,
    pub message: String,
    pub code: Option<String>,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

impl Diagnostic {
    pub fn error(location: Location, message: impl Into<String>) -> Self {
        Self {
            location,
            severity: Severity::Error,
            message: message.into(),
            code: None,
        }
    }

    pub fn warning(location: Location, message: impl Into<String>) -> Self {
        Self {
            location,
            severity: Severity::Warning,
            message: message.into(),
            code: None,
        }
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }
}