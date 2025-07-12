pub mod analysis;
pub mod analyzer;
pub mod diagnostics;
pub mod global_scope;
pub mod symbol_table;
pub mod validation;

pub use analyzer::{SemanticAnalyzer, SemanticInfo};
pub use diagnostics::{DiagnosticSeverity, SemanticDiagnostic};
pub use symbol_table::{Symbol, SymbolTable, SymbolType, ValueType};
