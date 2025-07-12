use aml_core::Location;

use crate::diagnostics::Diagnostics;
use crate::symbol_table::ValueType;

pub struct TypeChecker;

impl TypeChecker {
    pub fn validate_attribute_type(
        diagnostics: &mut Diagnostics,
        attribute_name: &str,
        actual_type: &ValueType,
        expected_type: &ValueType,
        location: Location,
    ) -> bool {
        if actual_type == expected_type || matches!(actual_type, ValueType::Unknown) {
            return true;
        }

        diagnostics.warning(
            location,
            format!("attribute '{attribute_name}' expects type {expected_type} but got {actual_type}. This attribute will be ignored.")
        );

        false
    }
}
