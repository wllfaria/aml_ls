use aml_core::Location;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SemanticDiagnostic {
    pub location: Location,
    pub message: String,
    pub severity: DiagnosticSeverity,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Default)]
pub struct Diagnostics(Vec<SemanticDiagnostic>);

impl Diagnostics {
    pub fn items(self) -> Vec<SemanticDiagnostic> {
        self.0
    }

    pub fn add_diagnostic(
        &mut self,
        location: Location,
        message: String,
        severity: DiagnosticSeverity,
    ) {
        self.0.push(SemanticDiagnostic {
            location,
            message,
            severity,
        });
    }

    pub fn error(&mut self, location: Location, message: impl Into<String>) {
        self.add_diagnostic(location, message.into(), DiagnosticSeverity::Error);
    }

    pub fn warning(&mut self, location: Location, message: impl Into<String>) {
        self.add_diagnostic(location, message.into(), DiagnosticSeverity::Warning);
    }

    pub fn info(&mut self, location: Location, message: impl Into<String>) {
        self.add_diagnostic(location, message.into(), DiagnosticSeverity::Info);
    }
}
