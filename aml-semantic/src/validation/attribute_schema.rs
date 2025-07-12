use std::collections::HashMap;

use aml_syntax::ast::*;

use crate::symbol_table::ValueType;

pub trait AttributeSchema {
    fn get_expected_attributes(&self) -> HashMap<&'static str, ValueType>;
}

impl AttributeSchema for Text {
    fn get_expected_attributes(&self) -> HashMap<&'static str, ValueType> {
        let mut attrs = HashMap::new();
        attrs.insert("foreground", ValueType::Hex);
        attrs.insert("background", ValueType::Hex);
        attrs.insert("text_align", ValueType::String);
        attrs.insert("wrap", ValueType::String);
        attrs
    }
}

impl AttributeSchema for Span {
    fn get_expected_attributes(&self) -> HashMap<&'static str, ValueType> {
        let mut attrs = HashMap::new();
        attrs.insert("foreground", ValueType::Hex);
        attrs.insert("background", ValueType::Hex);
        attrs.insert("text_align", ValueType::String);
        attrs.insert("wrap", ValueType::String);
        attrs
    }
}
