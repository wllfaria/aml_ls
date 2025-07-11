use aml_token::{Element, Operator, Token, TokenKind, Tokens};

use crate::ast::*;
use crate::expressions::parse_expression;

#[cfg(test)]
mod tests;

#[cfg(test)]
pub(crate) mod snapshots;

struct LocationCalculator;

impl LocationCalculator {
    fn merge_location_with_values(
        start: aml_core::Location,
        attributes: &Attributes,
        values: Option<&Vec<AstNode>>,
        children: Option<&Vec<AstNode>>,
    ) -> aml_core::Location {
        let value_location = values.and_then(|v| v.iter().last().map(|node| node.location()));
        let children_location = children.and_then(|c| c.iter().last().map(|node| node.location()));

        match (children_location, value_location, attributes.location) {
            (Some(location), _, _) => start.merge(location),
            (_, Some(location), _) => start.merge(location),
            (_, _, Some(location)) => start.merge(location),
            (None, None, None) => start,
        }
    }

    fn merge_location_without_values(
        start: aml_core::Location,
        attributes: &Attributes,
        children: &[AstNode],
    ) -> aml_core::Location {
        let children_location = children.iter().last().map(|node| node.location());

        match (children_location, attributes.location) {
            (Some(location), _) => start.merge(location),
            (_, Some(location)) => start.merge(location),
            (None, None) => start,
        }
    }
}

pub struct Parser {
    scope_stack: Vec<usize>,
    tokens: Tokens,
    ast: Ast,
}

impl Parser {
    pub fn new(mut tokens: Tokens) -> Self {
        tokens.consume_newlines();

        Self {
            tokens,
            ast: Ast::default(),
            scope_stack: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Ast {
        let base_indent = match self.tokens.peek().kind() {
            TokenKind::Indent(indent) => indent,
            _ => 0,
        };

        self.ast.nodes = self.parse_block(base_indent);
        self.ast
    }

    fn parse_block(&mut self, block_indent: usize) -> Vec<AstNode> {
        self.add_scope();

        let mut nodes = vec![];

        loop {
            self.tokens.consume_newlines();

            let current_indent = match self.tokens.peek().kind() {
                TokenKind::Indent(i) => {
                    self.tokens.consume();
                    i
                }
                _ => 0,
            };

            if current_indent < block_indent {
                break;
            }

            if current_indent > block_indent {
                break;
            }

            if self.tokens.peek().kind() == TokenKind::Eof {
                break;
            }

            nodes.push(self.parse_node(current_indent));
        }

        self.pop_scope();
        nodes
    }

    fn parse_node(&mut self, current_indent: usize) -> AstNode {
        let token = self.tokens.peek_skip_indent();
        match token.kind() {
            TokenKind::Element(element) => self.parse_element(element, current_indent),
            TokenKind::Container(_) => self.parse_container(current_indent),
            TokenKind::String(_) => self.parse_string(),
            TokenKind::Identifier(_) => self.parse_identifier(),
            TokenKind::Decl => self.parse_declaration(),
            TokenKind::Local => self.parse_declaration(),
            TokenKind::Global => self.parse_declaration(),
            TokenKind::Component => self.parse_component(),
            TokenKind::ComponentSlot => self.parse_component_slot(),
            TokenKind::For => self.parse_for_loop(current_indent),
            TokenKind::If => self.parse_if_statement(current_indent),
            TokenKind::Switch => self.parse_switch_statement(current_indent),
            TokenKind::With => todo!(),
            _ => AstNode::Error(ErrorNode {
                token: token.kind(),
                location: token.location(),
            }),
        }
    }

    fn parse_element(&mut self, element: Element, current_indent: usize) -> AstNode {
        match element {
            Element::Text => self.parse_text(current_indent),
            Element::Span => self.parse_span(),
        }
    }

    fn parse_component_slot(&mut self) -> AstNode {
        let component = self.tokens.next_token();
        let start_location = component.location();
        let name = self.parse_identifier();

        let location = start_location.merge(name.location());
        AstNode::ComponentSlot(ComponentSlot {
            location,
            name: Box::new(name),
        })
    }

    fn parse_component(&mut self) -> AstNode {
        let component = self.tokens.next_token();
        let start_location = component.location();

        let name = self.parse_identifier();
        let attributes = self.maybe_parse_attributes();

        let location = match attributes.location {
            Some(location) => start_location.merge(location),
            None => start_location.merge(name.location()),
        };

        AstNode::Component(Component {
            name: Box::new(name),
            location,
            attributes,
        })
    }

    fn parse_text(&mut self, current_indent: usize) -> AstNode {
        let text = self.tokens.next_token();
        assert!(text.kind() == TokenKind::Element(Element::Text));

        let start_location = text.location();
        let attributes = self.maybe_parse_attributes();
        let values = self.parse_values();
        let children = self.maybe_parse_block(current_indent);

        let location = LocationCalculator::merge_location_with_values(
            start_location,
            &attributes,
            Some(&values),
            Some(&children),
        );

        AstNode::Text(Text {
            values,
            attributes,
            children,
            location,
            keyword: text.location(),
        })
    }

    fn parse_span(&mut self) -> AstNode {
        let span = self.tokens.next_token();
        assert!(span.kind() == TokenKind::Element(Element::Span));

        let start_location = span.location();
        let attributes = self.maybe_parse_attributes();
        let values = self.parse_values();

        let location =
            LocationCalculator::merge_location_without_values(start_location, &attributes, &values);

        AstNode::Span(Span {
            values,
            attributes,
            location,
            keyword: span.location(),
        })
    }

    fn parse_container(&mut self, current_indent: usize) -> AstNode {
        let token = self.tokens.next_token();
        let kind = token.kind().expect_container();
        let start_location = token.location();
        let attributes = self.maybe_parse_attributes();
        let children = self.maybe_parse_block(current_indent);

        let location = LocationCalculator::merge_location_without_values(
            start_location,
            &attributes,
            &children,
        );

        AstNode::Container(ContainerNode {
            kind,
            children,
            attributes,
            location,
            keyword: token.location(),
        })
    }

    fn parse_for_loop(&mut self, current_indent: usize) -> AstNode {
        let keyword = self.tokens.next_token();
        let start_location = keyword.location();
        self.tokens.consume_indent();

        let binding = self.parse_identifier();
        self.tokens.consume_indent();

        if self.tokens.peek().kind() == TokenKind::In {
            self.tokens.consume()
        }
        self.tokens.consume_indent();

        let value = parse_expression(&mut self.tokens);
        let children = self.maybe_parse_block(current_indent);

        let last_child_location = children.last().map(|node| node.location());

        let location = match (last_child_location, value.location()) {
            (Some(location), _) => start_location.merge(location),
            (_, location) => start_location.merge(location),
        };

        AstNode::For(For {
            binding: Box::new(binding),
            value,
            children,
            location,
            keyword: keyword.location(),
        })
    }

    fn parse_if_statement(&mut self, current_indent: usize) -> AstNode {
        let mut branches = vec![];
        let mut start_location = self.tokens.peek().location();

        while let Some(branch) = self.parse_conditional_branch(current_indent) {
            branches.push(branch);
        }

        let last_branch_location = branches.last().map(|branch| branch.location());
        let location = start_location.merge(last_branch_location.unwrap_or(start_location));

        AstNode::If(IfChain { branches, location })
    }

    fn parse_conditional_branch(&mut self, current_indent: usize) -> Option<ConditionalBranch> {
        match self.tokens.peek().kind() {
            TokenKind::If => Some(ConditionalBranch::If(self.parse_if_node(current_indent))),
            TokenKind::Else => {
                let next_token = self.tokens.next_token();
                let keyword = next_token.location();
                self.tokens.consume_indent();
                assert!(next_token.kind() == TokenKind::Else);

                match self.tokens.peek().kind() {
                    TokenKind::If => {
                        let if_node = self.parse_if_node(current_indent);
                        Some(ConditionalBranch::ElseIf(keyword, if_node))
                    }
                    _ => {
                        let children = self.maybe_parse_block(current_indent);
                        let last_child_location = children.last().map(|node| node.location());
                        let location = keyword.merge(last_child_location.unwrap_or(keyword));

                        Some(ConditionalBranch::Else(Else {
                            children,
                            location,
                            keyword,
                        }))
                    }
                }
            }
            _ => None,
        }
    }

    fn parse_if_node(&mut self, current_indent: usize) -> If {
        let keyword = self.tokens.next_token().location();
        self.tokens.consume_indent();
        let condition = parse_expression(&mut self.tokens);
        let then = self.maybe_parse_block(current_indent);

        let then_location = then.last().map(|node| node.location());
        let condition_location = condition.location();
        let location = keyword.merge(then_location.unwrap_or(condition_location));

        If {
            then,
            keyword,
            location,
            condition,
        }
    }

    fn parse_values(&mut self) -> Vec<AstNode> {
        let mut values = vec![];
        loop {
            let next_token = self.tokens.peek_skip_indent();
            match next_token.kind() {
                TokenKind::Newline => break,
                TokenKind::Eof => break,
                TokenKind::Identifier(_) => values.push(self.parse_identifier()),
                TokenKind::Primitive(_) => values.push(self.parse_primitive()),
                TokenKind::String(_) => values.push(self.parse_string()),
                token => {
                    self.tokens.consume();
                    values.push(AstNode::Error(ErrorNode {
                        token,
                        location: next_token.location(),
                    }))
                }
            }
        }
        values
    }

    fn parse_switch_statement(&mut self, current_indent: usize) -> AstNode {
        let keyword = self.tokens.next_token();
        let start_location = keyword.location();
        self.tokens.consume_indent();
        let condition = parse_expression(&mut self.tokens);

        let mut branches = vec![];
        while let Some(branch) = self.parse_switch_branch(current_indent) {
            branches.push(branch);
        }

        let last_branch_location = branches.last().map(|branch| branch.location());
        let location = start_location.merge(last_branch_location.unwrap_or(condition.location()));
        AstNode::Switch(SwitchChain { branches, location })
    }

    fn parse_switch_branch(&mut self, current_indent: usize) -> Option<SwitchBranch> {
        self.tokens.consume_newlines();

        match self.tokens.peek_skip_indent().kind() {
            TokenKind::Case => Some(self.parse_case_branch(current_indent)),
            TokenKind::Default => Some(self.parse_default_branch(current_indent)),
            _ => None,
        }
    }

    fn parse_case_branch(&mut self, current_indent: usize) -> SwitchBranch {
        let keyword = self.tokens.next_token().location();
        self.tokens.consume_indent();

        let condition = parse_expression(&mut self.tokens);
        self.tokens.consume_indent();

        let colon_token = self.parse_optional_colon();
        self.tokens.consume_indent();

        let children = self.parse_switch_children(current_indent);
        let location = self.calculate_switch_location(
            keyword,
            &children,
            colon_token,
            Some(condition.location()),
        );

        SwitchBranch::Case(SwitchCase {
            condition,
            children,
            location,
            keyword,
            has_colon: colon_token.is_some(),
        })
    }

    fn parse_default_branch(&mut self, current_indent: usize) -> SwitchBranch {
        let keyword = self.tokens.next_token().location();
        self.tokens.consume_indent();

        let colon_token = self.parse_optional_colon();
        self.tokens.consume_indent();

        let children = self.parse_switch_children(current_indent);
        let location = self.calculate_switch_location(keyword, &children, colon_token, None);

        SwitchBranch::Default(SwitchDefault {
            children,
            keyword,
            location,
            has_colon: colon_token.is_some(),
        })
    }

    fn parse_optional_colon(&mut self) -> Option<Token> {
        match self.tokens.peek().kind() {
            TokenKind::Operator(Operator::Colon) => Some(self.tokens.next_token()),
            _ => None,
        }
    }

    fn parse_switch_children(&mut self, current_indent: usize) -> Vec<AstNode> {
        match self.tokens.peek_skip_indent().kind() {
            kind if kind.is_declaration() => vec![self.parse_declaration()],
            TokenKind::Newline => self.maybe_parse_block(current_indent),
            TokenKind::Element(element) => {
                vec![self.parse_element(element, current_indent)]
            }
            _ => vec![],
        }
    }

    fn calculate_switch_location(
        &self,
        keyword: aml_core::Location,
        children: &[AstNode],
        colon_token: Option<aml_token::Token>,
        fallback_location: Option<aml_core::Location>,
    ) -> aml_core::Location {
        let last_child_location = children.last().map(|node| node.location());

        match (last_child_location, colon_token, fallback_location) {
            (Some(location), _, _) => keyword.merge(location),
            (_, Some(colon), _) => keyword.merge(colon.location()),
            (None, None, Some(fallback)) => keyword.merge(fallback),
            (None, None, None) => keyword,
        }
    }

    fn parse_primitive(&mut self) -> AstNode {
        let primitive = self.tokens.next_token();
        let TokenKind::Primitive(value) = primitive.kind() else { unreachable!() };
        let location = primitive.location();
        AstNode::Primitive(PrimitiveNode { location, value })
    }

    fn parse_string(&mut self) -> AstNode {
        let token = self.tokens.next_token();
        let location = token.location();
        AstNode::String(location)
    }

    fn parse_identifier(&mut self) -> AstNode {
        let token = self.tokens.next_token();
        let TokenKind::Identifier(location) = token.kind() else {
            return AstNode::Error(ErrorNode {
                location: token.location(),
                token: token.kind(),
            });
        };
        AstNode::Identifier(location)
    }

    fn parse_declaration(&mut self) -> AstNode {
        let keyword = self.tokens.next_token();
        let start_location = keyword.location();

        let kind = match keyword.kind() {
            TokenKind::Local => DeclarationKind::Local,
            TokenKind::Decl => DeclarationKind::Local,
            TokenKind::Global => DeclarationKind::Global,
            _ => unreachable!(),
        };

        self.tokens.consume_indent();
        let name = self.parse_identifier();

        self.tokens.consume_indent();
        self.tokens.consume(); // consume equal sign
        let value = parse_expression(&mut self.tokens);

        let location = start_location.merge(value.location());
        AstNode::Declaration(Declaration {
            kind,
            value,
            location,
            name: Box::new(name),
        })
    }

    fn maybe_parse_attributes(&mut self) -> Attributes {
        self.tokens.consume_indent();
        if self.tokens.peek_skip_indent().kind() == TokenKind::Operator(Operator::LBracket) {
            let attributes = self.parse_attributes();
            self.tokens.consume_indent();
            return attributes;
        }

        Attributes::default()
    }

    fn maybe_parse_block(&mut self, current_indent: usize) -> Vec<AstNode> {
        self.tokens.consume_newlines();

        let next_indent = match self.tokens.peek().kind() {
            TokenKind::Indent(i) => i,
            _ => 0,
        };

        match next_indent > current_indent {
            true => self.parse_block(next_indent),
            false => vec![],
        }
    }

    fn parse_attributes(&mut self) -> Attributes {
        let token = self.tokens.next_token();
        let start_location = token.location();
        assert!(token.kind() == TokenKind::Operator(Operator::LBracket));

        let mut attributes = vec![];
        let end_location = loop {
            let next_token = self.tokens.peek_skip_indent();

            match next_token.kind() {
                TokenKind::Operator(Operator::RBracket) => {
                    self.tokens.consume();
                    break next_token.location();
                }
                TokenKind::Eof => break next_token.location(),
                TokenKind::Newline => {
                    self.tokens.consume();
                    continue;
                }
                _ => {}
            }

            let name = self.parse_identifier();
            self.tokens.consume_all_whitespace();

            // TODO: this is a syntax error if there is no colon
            if self.tokens.peek().kind() == TokenKind::Operator(Operator::Colon) {
                self.tokens.consume();
            }

            self.tokens.consume_all_whitespace();
            let value = parse_expression(&mut self.tokens);

            if value.has_error() {
                loop {
                    match self.tokens.peek().kind() {
                        TokenKind::Operator(Operator::RBracket) => break,
                        TokenKind::Eof => break,
                        _ => self.tokens.consume(),
                    }
                }
            }

            let location = name.location().merge(value.location());
            attributes.push(AstNode::Attribute(Attribute {
                name: Box::new(name),
                value,
                location,
            }));

            self.skip_optional_comma();
        };

        Attributes {
            items: attributes,
            location: Some(start_location.merge(end_location)),
        }
    }

    fn skip_optional_comma(&mut self) {
        if self.tokens.peek_skip_indent().kind() == TokenKind::Operator(Operator::Comma) {
            self.tokens.consume();
        }
    }

    fn add_scope(&mut self) {
        let scope_id = self.ast.scopes.len();
        let parent = self.scope_stack.last().copied();

        self.ast.scopes.push(Scope {
            variables: Vec::new(),
            parent,
        });

        self.scope_stack.push(scope_id);
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }
}
