use aml_core::Location;
use aml_syntax::ast::*;

use super::node_analyzer::AnalysisCtx;

#[derive(Debug)]
pub struct TextValidator<'src> {
    content: &'src str,
}

impl<'src> TextValidator<'src> {
    pub fn new(content: &'src str) -> Self {
        Self { content }
    }

    pub fn validate_text_element_value(
        &self,
        values: &[AstNode],
        location: Location,
        ctx: &mut AnalysisCtx<'_>,
    ) {
        if values.is_empty() {
            return ctx
                .diagnostics
                .warning(location, "Text element has no value to display");
        }

        for value in values {
            match value {
                // Strings and Primitives are allowed on text elements, so nothing to do here
                AstNode::String(_) | AstNode::Primitive(_) => {}
                // Identifiers are allowed on text elements, but they follow some rules
                AstNode::Identifier(_) => {
                    let name = self.get_node_text(value);

                    // Theoretically, if the parser successfully parsed an identifier, it should be
                    // valid. But take this as an assertion.
                    let Some(name) = name else {
                        ctx.diagnostics
                            .error(value.location(), "invalid identifier name");
                        continue;
                    };

                    // Users can shadow globals and locals, and if so, the local declaration has
                    // priority. We check if the identifier exists on local scope first, and if so,
                    // we can skip the global scope check.
                    if ctx.symbol_table.lookup_symbol(name).is_some() {
                        continue;
                    }

                    if ctx.global_scope.lookup_symbol(name).is_some() {
                        continue;
                    }

                    ctx.diagnostics.error(
                        location,
                        format!("reference to undefined identifier '{name:?}'"),
                    );
                }
                // TODO(wiru): text elements should receive expressions as values, as there are
                // cases where you can use them, like `text [] [1,2,3]`. Oh well...
                _ => ctx
                    .diagnostics
                    .error(location, "Text element value must be a string literal"),
            }
        }
    }

    fn get_node_text(&self, node: &AstNode) -> Option<&'src str> {
        match node {
            AstNode::Identifier(location) => Some(&self.content[location.to_range()]),
            AstNode::String(location) => Some(&self.content[location.to_range()]),
            _ => None,
        }
    }
}
