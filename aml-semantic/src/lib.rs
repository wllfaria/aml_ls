pub mod analyzer;
pub mod global_scope;
pub mod scope;
pub mod symbol_table;

pub use analyzer::{DiagnosticSeverity, SemanticAnalyzer, SemanticDiagnostic, SemanticInfo};
pub use scope::{ScopeAnalyzer, ScopeInfo};
pub use symbol_table::{Symbol, SymbolTable, SymbolType, ValueType};
