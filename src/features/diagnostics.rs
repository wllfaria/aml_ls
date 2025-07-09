use aml_core::Location;
use aml_semantic::{DiagnosticSeverity as SemanticDiagnosticSeverity, SemanticDiagnostic};
use tower_lsp::lsp_types::*;

use crate::core::document_manager::DocumentManager;

#[derive(Debug)]
pub struct DiagnosticProvider;

impl DiagnosticProvider {
    pub fn new() -> Self {
        Self
    }

    pub async fn get_diagnostics(
        &self,
        document_manager: &DocumentManager,
        uri: &Url,
    ) -> Vec<Diagnostic> {
        let files = document_manager.files().read().await;
        let Some(file_info) = files.get(uri) else {
            return Vec::new();
        };

        file_info
            .semantic_info
            .diagnostics
            .iter()
            .map(|diag| {
                self.convert_semantic_diagnostic(diag, document_manager, &file_info.content)
            })
            .collect()
    }

    fn convert_semantic_diagnostic(
        &self,
        semantic_diag: &SemanticDiagnostic,
        _document_manager: &DocumentManager,
        content: &str,
    ) -> Diagnostic {
        let range = self.location_to_range(semantic_diag.location, content);
        let severity = self.convert_severity(&semantic_diag.severity);

        Diagnostic {
            range,
            severity: Some(severity),
            code: None,
            code_description: None,
            source: Some("aml-ls".to_string()),
            message: semantic_diag.message.clone(),
            related_information: None,
            tags: None,
            data: None,
        }
    }

    fn location_to_range(&self, location: Location, content: &str) -> Range {
        let start_pos = DocumentManager::byte_offset_to_position(content, location.start_byte);
        let end_pos = DocumentManager::byte_offset_to_position(content, location.end_byte);

        Range {
            start: start_pos,
            end: end_pos,
        }
    }

    fn convert_severity(&self, severity: &SemanticDiagnosticSeverity) -> DiagnosticSeverity {
        match severity {
            SemanticDiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
            SemanticDiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
            SemanticDiagnosticSeverity::Info => DiagnosticSeverity::INFORMATION,
        }
    }
}
