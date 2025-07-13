use aml_core::Location;
use aml_syntax::ast::*;
use aml_token::Container;

use super::color::is_valid_color;
use crate::DiagnosticSeverity;
use crate::diagnostics::Diagnostics;
use crate::symbol_table::ValueType;

macro_rules! combine {
    ($a:expr, $b:expr $(, $c:expr)* $(,)?) => {{
        let mut combined = Vec::with_capacity($a.len() + $b.len() $(+ $c.len())*);
        combined.extend_from_slice($a);
        combined.extend_from_slice($b);
        $(combined.extend_from_slice($c);)*
        combined
    }}
}

#[derive(Debug, Clone)]
pub struct ValidationRule {
    pub code: &'static str,
    pub message_template: &'static str,
    pub severity: DiagnosticSeverity,
}

#[derive(Debug, Clone)]
pub enum AttributeValidatorKind {
    Int,
    Color,
    String,
    Boolean,
    StringEnum(&'static [&'static str]),
}

#[derive(Debug, Clone)]
pub struct AttributeSchema {
    pub name: &'static str,
    pub validator: AttributeValidatorKind,
    pub description: &'static str,
}

pub trait ElementSchema {
    fn supported_attributes(&self) -> Vec<AttributeSchema>;
    fn get_attribute_schema(&self, name: &str) -> Option<AttributeSchema> {
        self.supported_attributes()
            .into_iter()
            .find(|attr| attr.name == name)
    }
}

pub const VALIDATION_RULES: &[ValidationRule] = &[
    ValidationRule {
        code: "E001",
        message_template: "invalid color value, expected a hex value or a color name, but got '{0}'",
        severity: DiagnosticSeverity::Error,
    },
    ValidationRule {
        code: "E002",
        message_template: "invalid value type for attribute '{0}', expected a hex value or a color name, but got '{1}'",
        severity: DiagnosticSeverity::Error,
    },
    ValidationRule {
        code: "E003",
        message_template: "invalid value type for attribute '{0}', expected a string value, but got '{1}'",
        severity: DiagnosticSeverity::Error,
    },
    ValidationRule {
        code: "E004",
        message_template: "invalid value type for attribute '{0}', expected an integer value, but got '{1}'",
        severity: DiagnosticSeverity::Error,
    },
    ValidationRule {
        code: "W001",
        message_template: "invalid value type for attribute '{0}', expected a boolean value, but got '{1}'. This attribute will be ignored",
        severity: DiagnosticSeverity::Warning,
    },
    ValidationRule {
        code: "W002",
        message_template: "invalid value for attribute '{0}', expected one of [{1}], but got '{2}'. This attribute will be ignored",
        severity: DiagnosticSeverity::Warning,
    },
    ValidationRule {
        code: "W003",
        message_template: "invalid value type for attribute '{0}', expected one of '{1}', but got '{2}'. This attribute will be ignored",
        severity: DiagnosticSeverity::Warning,
    },
];

pub const SHARED_ATTRIBUTES: &[AttributeSchema] = &[
    AttributeSchema {
        name: "display",
        validator: AttributeValidatorKind::StringEnum(&["show", "hide", "exclude"]),
        description: "Controls element visibility in the rendered output",
    },
    AttributeSchema {
        name: "fill",
        validator: AttributeValidatorKind::String,
        description: "Content to fill the element with",
    },
    AttributeSchema {
        name: "foreground",
        validator: AttributeValidatorKind::Color,
        description: "Foreground color for text content",
    },
    AttributeSchema {
        name: "background",
        validator: AttributeValidatorKind::Color,
        description: "Background color for the element",
    },
];

pub const TEXT_SPECIFIC_ATTRIBUTES: &[AttributeSchema] = &[
    AttributeSchema {
        name: "text_align",
        validator: AttributeValidatorKind::StringEnum(&["left", "center", "right", "centre"]),
        description: "Text alignment within the element",
    },
    AttributeSchema {
        name: "wrap",
        validator: AttributeValidatorKind::StringEnum(&["break"]),
        description: "Text wrapping behavior",
    },
    AttributeSchema {
        name: "bold",
        validator: AttributeValidatorKind::Boolean,
        description: "Whether text should be bold",
    },
    AttributeSchema {
        name: "italic",
        validator: AttributeValidatorKind::Boolean,
        description: "Whether text should be italic",
    },
];

pub const SPAN_SPECIFIC_ATTRIBUTES: &[AttributeSchema] = &[
    // Span currently only uses shared attributes
];

const GENERAL_CONTAINER_ATTRIBUTES: &[AttributeSchema] = &[
    AttributeSchema {
        name: "width",
        validator: AttributeValidatorKind::Int,
        description: "Width of the container",
    },
    AttributeSchema {
        name: "height",
        validator: AttributeValidatorKind::Int,
        description: "Height of the container",
    },
    AttributeSchema {
        name: "min_width",
        validator: AttributeValidatorKind::Int,
        description: "Minimum width of the container",
    },
    AttributeSchema {
        name: "min_height",
        validator: AttributeValidatorKind::Int,
        description: "Minimum height of the container",
    },
];

pub const BORDER_SPECIFIC_ATTRIBUTES: &[AttributeSchema] = &[
    AttributeSchema {
        name: "sides",
        validator: AttributeValidatorKind::StringEnum(&["top", "right", "bottom", "left"]),
        description: "Sides of the border",
    },
    AttributeSchema {
        name: "border_style",
        validator: AttributeValidatorKind::String,
        description: "Border style",
    },
];

pub const ALIGNMENT_SPECIFIC_ATTRIBUTES: &[AttributeSchema] = &[AttributeSchema {
    name: "alignment",
    validator: AttributeValidatorKind::StringEnum(&[
        "top_left",
        "top",
        "top_right",
        "right",
        "bottom_right",
        "bottom",
        "bottom_left",
        "left",
        "centre",
        "center",
    ]),
    description: "Text alignment within the element",
}];

pub const EXPAND_SPECIFIC_ATTRIBUTES: &[AttributeSchema] = &[
    AttributeSchema {
        name: "factor",
        validator: AttributeValidatorKind::Int,
        description: "Factor to expand the container by",
    },
    AttributeSchema {
        name: "axis",
        validator: AttributeValidatorKind::StringEnum(&["horz", "horizontal", "vert", "vertical"]),
        description: "Axis to expand the container by",
    },
];

const DIRECTION_ATTRIBUTES: &[AttributeSchema] = &[
    AttributeSchema {
        name: "left",
        validator: AttributeValidatorKind::Int,
        description: "Left position of the container",
    },
    AttributeSchema {
        name: "top",
        validator: AttributeValidatorKind::Int,
        description: "Top position of the container",
    },
    AttributeSchema {
        name: "right",
        validator: AttributeValidatorKind::Int,
        description: "Right position of the container",
    },
    AttributeSchema {
        name: "bottom",
        validator: AttributeValidatorKind::Int,
        description: "Bottom position of the container",
    },
];

pub const POSITION_SPECIFIC_ATTRIBUTES: &[AttributeSchema] = &[AttributeSchema {
    name: "placement",
    validator: AttributeValidatorKind::StringEnum(&["relative", "absolute"]),
    description: "Placement of the container",
}];

const PADDING_SEPCIFIC_ATTRIBUTES: &[AttributeSchema] = &[AttributeSchema {
    name: "padding",
    validator: AttributeValidatorKind::Int,
    description: "Padding to apply to all sides of the container",
}];

const CANVAS_SPECIFIC_ATTRIBUTES: &[AttributeSchema] = &[
    AttributeSchema {
        name: "width",
        validator: AttributeValidatorKind::Int,
        description: "Width of the canvas",
    },
    AttributeSchema {
        name: "height",
        validator: AttributeValidatorKind::Int,
        description: "Height of the canvas",
    },
];

pub const CONTAINER_SPECIFIC_ATTRIBUTES: &[AttributeSchema] = &[
    AttributeSchema {
        name: "max_width",
        validator: AttributeValidatorKind::Int,
        description: "Maximum width of the container",
    },
    AttributeSchema {
        name: "max_height",
        validator: AttributeValidatorKind::Int,
        description: "Maximum height of the container",
    },
];

impl ElementSchema for Text {
    fn supported_attributes(&self) -> Vec<AttributeSchema> {
        combine!(SHARED_ATTRIBUTES, TEXT_SPECIFIC_ATTRIBUTES)
    }
}

impl ElementSchema for Span {
    fn supported_attributes(&self) -> Vec<AttributeSchema> {
        combine!(SHARED_ATTRIBUTES, SPAN_SPECIFIC_ATTRIBUTES)
    }
}

impl ElementSchema for ContainerNode {
    fn supported_attributes(&self) -> Vec<AttributeSchema> {
        match self.kind {
            Container::Border => combine!(
                SHARED_ATTRIBUTES,
                GENERAL_CONTAINER_ATTRIBUTES,
                BORDER_SPECIFIC_ATTRIBUTES,
            ),
            Container::Alignment => combine!(
                SHARED_ATTRIBUTES,
                GENERAL_CONTAINER_ATTRIBUTES,
                ALIGNMENT_SPECIFIC_ATTRIBUTES,
            ),
            Container::VStack => combine!(SHARED_ATTRIBUTES, GENERAL_CONTAINER_ATTRIBUTES),
            Container::HStack => combine!(SHARED_ATTRIBUTES, GENERAL_CONTAINER_ATTRIBUTES),
            Container::ZStack => combine!(SHARED_ATTRIBUTES, GENERAL_CONTAINER_ATTRIBUTES),
            Container::Expand => combine!(SHARED_ATTRIBUTES, EXPAND_SPECIFIC_ATTRIBUTES),
            Container::Position => combine!(
                SHARED_ATTRIBUTES,
                POSITION_SPECIFIC_ATTRIBUTES,
                DIRECTION_ATTRIBUTES
            ),
            Container::Padding => combine!(
                SHARED_ATTRIBUTES,
                DIRECTION_ATTRIBUTES,
                PADDING_SEPCIFIC_ATTRIBUTES
            ),
            Container::Canvas => combine!(SHARED_ATTRIBUTES, CANVAS_SPECIFIC_ATTRIBUTES),
            Container::Container => combine!(
                SHARED_ATTRIBUTES,
                CONTAINER_SPECIFIC_ATTRIBUTES,
                GENERAL_CONTAINER_ATTRIBUTES,
            ),
            _ => vec![],
        }
    }
}

fn validate_by_schema(ctx: &mut ValidationCtx<'_>, schema: &AttributeSchema) {
    match &schema.validator {
        AttributeValidatorKind::Color => validate_color_value(ctx),
        AttributeValidatorKind::Boolean => validate_boolean_value(ctx),
        AttributeValidatorKind::StringEnum(valid_values) => {
            validate_string_enum_value(ctx, valid_values)
        }
        AttributeValidatorKind::String => validate_string_value(ctx),
        AttributeValidatorKind::Int => validate_int_value(ctx),
    }
}

fn validate_color_value(ctx: &mut ValidationCtx<'_>) {
    let value_string = ctx.value_string.trim_matches('"').trim_matches('\'');
    match ctx.value_type {
        ValueType::Hex => {}
        ValueType::String if !is_valid_color(value_string) => {
            emit_diagnostic(ctx, "E001", &[value_string])
        }
        ValueType::Int if !is_valid_color(value_string) => {
            emit_diagnostic(ctx, "E001", &[value_string]);
        }
        ValueType::String => {}
        ValueType::Int => {}
        _ => emit_diagnostic(
            ctx,
            "E002",
            &[ctx.attribute_name, &ctx.value_type.to_string()],
        ),
    }
}

fn validate_boolean_value(ctx: &mut ValidationCtx<'_>) {
    match ctx.value_type {
        ValueType::Boolean => {}
        _ => emit_diagnostic(
            ctx,
            "W001",
            &[ctx.attribute_name, &ctx.value_type.to_string()],
        ),
    }
}

fn validate_string_enum_value(ctx: &mut ValidationCtx<'_>, valid_values: &[&str]) {
    let value_string = ctx.value_string.trim_matches('"').trim_matches('\'');
    match ctx.value_type {
        ValueType::String if valid_values.contains(&value_string) => {}
        ValueType::String => {
            let valid_list = valid_values.join(", ");
            emit_diagnostic(
                ctx,
                "W002",
                &[ctx.attribute_name, &valid_list, value_string],
            );
        }
        _ => {
            let valid_list = valid_values.join(", ");
            emit_diagnostic(
                ctx,
                "W003",
                &[ctx.attribute_name, &valid_list, &ctx.value_type.to_string()],
            );
        }
    }
}

fn validate_string_value(ctx: &mut ValidationCtx<'_>) {
    match ctx.value_type {
        ValueType::String => {}
        _ => emit_diagnostic(
            ctx,
            "E003",
            &[ctx.attribute_name, &ctx.value_type.to_string()],
        ),
    }
}

pub fn validate_int_value(ctx: &mut ValidationCtx<'_>) {
    match ctx.value_type {
        ValueType::Int => {}
        _ => emit_diagnostic(
            ctx,
            "E004",
            &[ctx.attribute_name, &ctx.value_type.to_string()],
        ),
    }
}

fn emit_diagnostic(ctx: &mut ValidationCtx<'_>, rule_code: &str, args: &[&str]) {
    if let Some(rule) = VALIDATION_RULES.iter().find(|r| r.code == rule_code) {
        let message = format_message(rule.message_template, args);
        match rule.severity {
            DiagnosticSeverity::Error => ctx.diagnostics.error(ctx.value_location, message),
            DiagnosticSeverity::Warning => ctx.diagnostics.warning(ctx.value_location, message),
            DiagnosticSeverity::Info => ctx.diagnostics.info(ctx.value_location, message),
            DiagnosticSeverity::Hint => ctx.diagnostics.hint(ctx.value_location, message),
        }
    }
}

fn format_message(template: &str, args: &[&str]) -> String {
    let mut result = template.to_string();
    for (i, arg) in args.iter().enumerate() {
        result = result.replace(&format!("{{{i}}}"), arg);
    }
    result
}

pub trait AttributeValidator {
    fn validate_attribute(&self, ctx: &mut ValidationCtx<'_>);
}

pub struct ValidationCtx<'src> {
    pub value_string: &'src str,
    pub value_location: Location,
    pub attribute_name: &'src str,
    pub value_type: &'src ValueType,
    pub diagnostics: &'src mut Diagnostics,
}

impl AttributeValidator for Text {
    fn validate_attribute(&self, ctx: &mut ValidationCtx<'_>) {
        if let Some(schema) = self.get_attribute_schema(ctx.attribute_name) {
            validate_by_schema(ctx, &schema);
        }
    }
}

impl AttributeValidator for Span {
    fn validate_attribute(&self, ctx: &mut ValidationCtx<'_>) {
        if let Some(schema) = self.get_attribute_schema(ctx.attribute_name) {
            validate_by_schema(ctx, &schema);
        }
    }
}

impl AttributeValidator for ContainerNode {
    fn validate_attribute(&self, ctx: &mut ValidationCtx<'_>) {
        if let Some(schema) = self.get_attribute_schema(ctx.attribute_name) {
            validate_by_schema(ctx, &schema);
        }
    }
}
