use std::collections::{HashMap, HashSet};

use aml_core::Location;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct Symbol {
    pub name: String,
    pub scope_id: usize,
    pub location: Location,
    pub symbol_type: SymbolType,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SymbolType {
    Variable(ValueType),
    Element,
}

impl std::fmt::Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolType::Variable(value_type) => write!(f, "{value_type}"),
            SymbolType::Element => write!(f, "element"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Hash)]
pub enum ValueType {
    String,
    Int,
    Float,
    Boolean,
    List(Vec<ValueType>),
    Map(Vec<(ValueType, ValueType)>),
    Hex,
    Unknown,
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::String => write!(f, "string"),
            ValueType::Int => write!(f, "int"),
            ValueType::Float => write!(f, "float"),
            ValueType::Boolean => write!(f, "bool"),
            ValueType::List(value_types) => {
                let distinct_types = value_types
                    .iter()
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .collect::<Vec<_>>();

                let mut list_type = String::from("list<");

                for (idx, t) in distinct_types.iter().enumerate() {
                    list_type.push_str(&format!("{t}"));
                    if idx < distinct_types.len() - 1 {
                        list_type.push_str(" | ");
                    }
                }

                list_type.push('>');

                write!(f, "{list_type}")
            }
            ValueType::Map(entry_types) => {
                let distinct_types = entry_types
                    .iter()
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .collect::<Vec<_>>();

                let mut map_type = String::from("map<");

                for (idx, (_, v)) in distinct_types.iter().enumerate() {
                    map_type.push_str(&format!("{v}"));
                    if idx < distinct_types.len() - 1 {
                        map_type.push_str(" | ");
                    }
                }

                map_type.push('>');

                write!(f, "{map_type}")
            }
            ValueType::Hex => write!(f, "hex"),
            ValueType::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
    current_scope: usize,
    symbols: HashMap<String, Vec<Symbol>>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub id: usize,
    pub symbols: Vec<String>,
    pub parent: Option<usize>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut table = Self {
            symbols: HashMap::new(),
            scopes: Vec::new(),
            current_scope: 0,
        };

        table.push_scope(None);
        table
    }

    pub fn push_scope(&mut self, parent: Option<usize>) -> usize {
        let scope_id = self.scopes.len();
        let parent = parent.or(if scope_id == 0 { None } else { Some(self.current_scope) });

        self.scopes.push(Scope {
            id: scope_id,
            parent,
            symbols: Vec::new(),
        });

        self.current_scope = scope_id;
        scope_id
    }

    pub fn pop_scope(&mut self) {
        if let Some(parent) = self.scopes[self.current_scope].parent {
            self.current_scope = parent;
        }
    }

    pub fn declare_symbol(&mut self, name: String, location: Location, symbol_type: SymbolType) {
        let symbol = Symbol {
            name: name.clone(),
            location,
            symbol_type,
            scope_id: self.current_scope,
        };

        self.scopes[self.current_scope].symbols.push(name.clone());
        self.symbols.entry(name).or_default().push(symbol);
    }

    pub fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        let symbols = self.symbols.get(name)?;

        let mut scope_id = Some(self.current_scope);
        while let Some(id) = scope_id {
            // we can shadow variables in the same scope, so when finding a symbol, we need to
            // return the last one we find, since it's the most recently declared one
            if let Some(symbol) = symbols.iter().rev().find(|s| s.scope_id == id) {
                return Some(symbol);
            }
            scope_id = self.scopes[id].parent;
        }

        None
    }

    pub fn get_symbols_in_scope(&self, scope_id: usize) -> Vec<&Symbol> {
        let mut result = Vec::new();

        if let Some(scope) = self.scopes.get(scope_id) {
            for symbol_name in &scope.symbols {
                if let Some(symbols) = self.symbols.get(symbol_name)
                    && let Some(symbol) = symbols.iter().find(|s| s.scope_id == scope_id)
                {
                    result.push(symbol);
                }
            }
        }

        result
    }

    pub fn find_references(&self, name: &str) -> Vec<&Symbol> {
        self.symbols
            .get(name)
            .map_or(Vec::new(), |symbols| symbols.iter().collect())
    }

    pub fn get_scope(&self, scope_id: usize) -> Option<&Scope> {
        self.scopes.get(scope_id)
    }

    pub fn current_scope_id(&self) -> usize {
        self.current_scope
    }

    pub fn find_symbol_at_position(&self, position: Location) -> Option<&Symbol> {
        for symbols in self.symbols.values() {
            for symbol in symbols {
                if position.start_byte >= symbol.location.start_byte
                    && position.start_byte <= symbol.location.end_byte
                {
                    return Some(symbol);
                }
            }
        }
        None
    }
}
