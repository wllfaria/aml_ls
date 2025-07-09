use std::collections::HashMap;
use std::path::PathBuf;

use aml_core::Location;

use crate::SymbolType;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlobalSymbol {
    pub name: String,
    pub location: Location,
    pub symbol_type: SymbolType,
    pub definition: PathBuf,
}

#[derive(Debug, Default)]
pub struct GlobalScope {
    pub globals: HashMap<String, GlobalSymbol>,
}

impl GlobalScope {
    pub fn new() -> Self {
        Self {
            globals: Default::default(),
        }
    }

    pub fn declare_global(&mut self, symbol: GlobalSymbol) {
        if self.globals.contains_key(&symbol.name) {
            return;
        };

        let name = symbol.name.clone();
        self.globals.insert(name, symbol);
    }

    pub fn lookup_symbol(&self, name: &str) -> Option<&GlobalSymbol> {
        self.globals.get(name)
    }
}
