use aml_syntax::ast::*;

use crate::ValueType;
use crate::analysis::text_validation::TextValidator;
use crate::diagnostics::{DiagnosticSeverity, Diagnostics};
use crate::global_scope::GlobalScope;
use crate::symbol_table::{SymbolTable, SymbolType};
use crate::validation::attribute_schema::{AttributeValidator, ValidationCtx};
use crate::validation::expression::ExpressionAnalyzer;

#[derive(Debug)]
pub struct AnalysisCtx<'src> {
    pub node: &'src AstNode,
    pub parent: Option<&'src AstNode>,
    pub symbol_table: &'src mut SymbolTable,
    pub global_scope: &'src GlobalScope,
    pub diagnostics: &'src mut Diagnostics,
}

#[derive(Debug)]
pub struct NodeAnalyzer<'src> {
    content: &'src str,
    text_validator: TextValidator<'src>,
    expression_analyzer: ExpressionAnalyzer<'src>,
}

impl<'src> NodeAnalyzer<'src> {
    pub fn new(content: &'src str) -> Self {
        Self {
            content,
            text_validator: TextValidator::new(content),
            expression_analyzer: ExpressionAnalyzer::new(content),
        }
    }

    pub fn analyze_node(&self, ctx: &mut AnalysisCtx<'src>) {
        match ctx.node {
            AstNode::Text(text) => self.analyze_text_element(text, ctx),
            AstNode::Span(span) => self.analyze_span_element(span, ctx),
            AstNode::ComponentSlot(slot) => self.analyze_slot(slot, ctx),
            AstNode::Container(container) => self.analyze_container(container, ctx),
            AstNode::Component(component) => self.analyze_component(component, ctx),
            AstNode::Attribute(attribute) => self.analyze_attribute(attribute, ctx),
            AstNode::If(if_chain) => self.analyze_if_chain(if_chain, ctx),

            AstNode::For(_) => {}
            AstNode::With(_) => {}
            AstNode::Switch(_) => {}

            AstNode::Declaration(declaration) if declaration.is_global() => {
                // Global declarations are collected before analyzing as they can be used before
                // being defined. Nothing to do here.
            }
            AstNode::Declaration(declaration) => {
                // Local declarations are collected as they are defined and then hoisted to the
                // root scope. They cannot be used before being defined.
                self.declare_variable(declaration, ctx);
            }

            AstNode::String(_) => {}
            AstNode::Primitive(_) => {}
            AstNode::Identifier(_) => {}
            AstNode::Error(error) => ctx.diagnostics.add_diagnostic(
                error.location,
                format!("unexpected token '{:?}'", error.token),
                DiagnosticSeverity::Error,
            ),
        }
    }

    fn analyze_text_element(&self, text: &Text, ctx: &mut AnalysisCtx<'_>) {
        // Text elements can only be a direct child of a container, so if we have a parent, it
        // should be a container.
        if let Some(parent) = ctx.parent {
            if !matches!(parent, AstNode::Container(_)) {
                ctx.diagnostics.warning(
                    text.location,
                    "Text elements can only be direct children of a container. This element will be ignored",
                );
            }
        }

        self.text_validator
            .validate_text_element_value(&text.values, text.location, ctx);

        for attr in text.attributes.items.iter() {
            self.analyze_node(&mut AnalysisCtx {
                node: attr,
                parent: Some(ctx.node),
                symbol_table: ctx.symbol_table,
                global_scope: ctx.global_scope,
                diagnostics: ctx.diagnostics,
            });
        }

        for child in text.children.iter() {
            self.analyze_node(&mut AnalysisCtx {
                node: child,
                parent: Some(ctx.node),
                symbol_table: ctx.symbol_table,
                global_scope: ctx.global_scope,
                diagnostics: ctx.diagnostics,
            });
        }
    }

    fn analyze_span_element(&self, span: &Span, ctx: &mut AnalysisCtx<'_>) {
        // Span elements are ignored if they are not a direct child of a text element
        if !matches!(ctx.parent, Some(AstNode::Text(_))) {
            ctx.diagnostics.warning(
                span.location,
                "Span elements can only be a direct children of a text. This element will be ignored",
            );

            ctx.diagnostics.hint(
                span.location,
                "If you meant to style text within the same line, consider adding a text element with an empty string as the span parent",
            );
        }

        for attr in span.attributes.items.iter() {
            self.analyze_node(&mut AnalysisCtx {
                node: attr,
                parent: None,
                symbol_table: ctx.symbol_table,
                global_scope: ctx.global_scope,
                diagnostics: ctx.diagnostics,
            });
        }

        for value in span.values.iter() {
            self.analyze_node(&mut AnalysisCtx {
                node: value,
                parent: None,
                symbol_table: ctx.symbol_table,
                global_scope: ctx.global_scope,
                diagnostics: ctx.diagnostics,
            });
        }
    }

    fn analyze_container(&self, container: &ContainerNode, ctx: &mut AnalysisCtx<'_>) {
        for attr in container.attributes.items.iter() {
            self.analyze_node(&mut AnalysisCtx {
                node: attr,
                parent: Some(ctx.node),
                symbol_table: ctx.symbol_table,
                global_scope: ctx.global_scope,
                diagnostics: ctx.diagnostics,
            });
        }

        for child in container.children.iter() {
            self.analyze_node(&mut AnalysisCtx {
                node: child,
                parent: Some(ctx.node),
                symbol_table: ctx.symbol_table,
                global_scope: ctx.global_scope,
                diagnostics: ctx.diagnostics,
            });
        }
    }

    fn analyze_component(&self, component: &Component, ctx: &mut AnalysisCtx<'_>) {
        if component.name.has_error() {
            return ctx.diagnostics.error(
                component.name.location(),
                "component names must be valid identifiers",
            );
        }

        let name = self
            .get_node_text(&component.name)
            .expect("component name is guaranteed to be an identifier");

        for attr in component.attributes.items.iter() {
            self.analyze_node(&mut AnalysisCtx {
                node: attr,
                parent: Some(ctx.node),
                symbol_table: ctx.symbol_table,
                global_scope: ctx.global_scope,
                diagnostics: ctx.diagnostics,
            });
        }
    }

    fn analyze_attribute(&self, attribute: &Attribute, ctx: &mut AnalysisCtx<'src>) {
        let parent = ctx
            .parent
            .expect("attributes can't be used outside of a node");

        // Attribute names are parsed as identifiers only, and otherwise they are parsed as error
        // nodes by the parser, so we only need to check if its an Error, and if not, its a valid
        // identifier.
        if let AstNode::Error(error) = attribute.name.as_ref() {
            return ctx.diagnostics.error(
                attribute.name.location(),
                format!("attribute names must be identifiers, got '{}'", error.token),
            );
        }

        let attribute_type = self
            .expression_analyzer
            .analyze_expression(&attribute.value, ctx);

        let attribute_name = self
            .get_node_text(&attribute.name)
            .expect("attribute is guaranteed to be an identifier");

        let mut validation_ctx = ValidationCtx {
            value_string: &self.content[attribute.value.location().to_range()],
            attribute_name,
            value_type: &attribute_type,
            diagnostics: ctx.diagnostics,
            value_location: attribute.value.location(),
        };

        // Validate attributes against their expected types for specific node types
        match parent {
            AstNode::Text(text) => text.validate_attribute(&mut validation_ctx),
            AstNode::Span(span) => span.validate_attribute(&mut validation_ctx),
            AstNode::Container(container) => container.validate_attribute(&mut validation_ctx),
            _ => {}
        }
    }

    fn analyze_if_chain(&self, if_chain: &IfChain, ctx: &mut AnalysisCtx<'_>) {
        for branch in if_chain.branches.iter() {
            match branch {
                ConditionalBranch::If(if_stmt) => {
                    let condition_type = self
                        .expression_analyzer
                        .analyze_expression(&if_stmt.condition, ctx);

                    if !matches!(condition_type, ValueType::Boolean) {
                        ctx.diagnostics.error(
                            if_stmt.condition.location(),
                            "If statement condition must evaluate to a boolean",
                        );
                    }

                    for child in if_stmt.then.iter() {
                        self.analyze_node(&mut AnalysisCtx {
                            node: child,
                            parent: Some(ctx.node),
                            symbol_table: ctx.symbol_table,
                            global_scope: ctx.global_scope,
                            diagnostics: ctx.diagnostics,
                        });
                    }
                }
                ConditionalBranch::ElseIf(_, if_stmt) => {}
                ConditionalBranch::Else(else_stmt) => {}
            }
        }
    }

    fn analyze_slot(&self, slot: &ComponentSlot, ctx: &mut AnalysisCtx<'_>) {
        self.analyze_node(&mut AnalysisCtx {
            node: &slot.name,
            parent: None,
            symbol_table: ctx.symbol_table,
            global_scope: ctx.global_scope,
            diagnostics: ctx.diagnostics,
        });
    }

    fn declare_variable(&self, declaration: &Declaration, ctx: &mut AnalysisCtx<'src>) {
        let Some(name) = self.get_node_text(&declaration.name) else {
            return ctx
                .diagnostics
                .error(declaration.name.location(), "invalid identifier name");
        };
        let value_type = self
            .expression_analyzer
            .analyze_expression(&declaration.value, ctx);
        let symbol_type = SymbolType::Variable(value_type);

        ctx.symbol_table
            .declare_symbol(name.into(), declaration.location, symbol_type);
    }

    fn get_node_text(&self, node: &AstNode) -> Option<&'src str> {
        match node {
            AstNode::Identifier(location) => Some(&self.content[location.to_range()]),
            AstNode::String(location) => Some(&self.content[location.to_range()]),
            _ => None,
        }
    }
}
