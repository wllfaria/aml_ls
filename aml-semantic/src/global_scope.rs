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
        let name = symbol.name.clone();
        self.globals.insert(name, symbol);
    }

    // pub async fn get_global_location(&self, name: &str) -> Option<PathBuf> {
    //     let globals = self.globals.read().await;
    //     globals.get(symbol).cloned()
    // }
    //
    // pub async fn is_global_defined(&self, symbol: &str) -> bool {
    //     let globals = self.globals.read().await;
    //     globals.contains_key(symbol)
    // }
}
