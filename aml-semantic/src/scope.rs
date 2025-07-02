use aml_core::Location;
use aml_syntax::{Ast, AstNode};

#[derive(Debug, Clone)]
pub struct ScopeInfo {
    pub id: usize,
    pub parent: Option<usize>,
    pub range: ScopeRange,
    pub variables: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ScopeRange {
    pub start: Location,
    pub end: Location,
}

#[derive(Debug, Default)]
pub struct ScopeAnalyzer {
    scopes: Vec<ScopeInfo>,
    current_scope: usize,
    scope_stack: Vec<usize>,
}

impl ScopeAnalyzer {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            current_scope: 0,
            scope_stack: Vec::new(),
        }
    }

    pub fn analyze(&mut self, ast: &Ast) -> Vec<ScopeInfo> {
        // Create root scope
        self.push_scope(None, Location::new(0, 0));

        // Analyze all nodes
        for node in &ast.nodes {
            self.analyze_node(node);
        }

        // Close root scope
        self.pop_scope(Location::new(usize::MAX, usize::MAX));

        std::mem::take(&mut self.scopes)
    }

    fn analyze_node(&mut self, node: &AstNode) {
        match node {
            AstNode::Text {
                children, location, ..
            } => {
                // Text elements create new scopes
                self.push_scope(Some(self.current_scope), *location);

                // Analyze children in this scope
                for child in children {
                    self.analyze_node(child);
                }

                self.pop_scope(*location);
            }
            AstNode::Span { attributes, .. } => {
                // Analyze attributes in current scope
                for attr in attributes {
                    self.analyze_node(attr);
                }
            }
            AstNode::Attribute { name, .. } => {
                // Attributes declare variables in current scope
                if let Some(var_name) = self.get_attribute_name(name) {
                    self.declare_variable(var_name);
                }
            }
            AstNode::Identifier { .. } | AstNode::String { .. } => {
                // These don't create scopes
            }
            AstNode::Declaration { .. } => {}
        }
    }

    fn push_scope(&mut self, parent: Option<usize>, start: Location) {
        let scope_id = self.scopes.len();

        self.scopes.push(ScopeInfo {
            id: scope_id,
            parent,
            range: ScopeRange { start, end: start }, // Will be updated when scope is closed
            variables: Vec::new(),
        });

        self.scope_stack.push(self.current_scope);
        self.current_scope = scope_id;
    }

    fn pop_scope(&mut self, end: Location) {
        // Update scope end location
        if let Some(scope) = self.scopes.get_mut(self.current_scope) {
            scope.range.end = end;
        }

        // Return to parent scope
        if let Some(parent_scope) = self.scope_stack.pop() {
            self.current_scope = parent_scope;
        }
    }

    fn declare_variable(&mut self, name: String) {
        if let Some(scope) = self.scopes.get_mut(self.current_scope)
            && !scope.variables.contains(&name)
        {
            scope.variables.push(name);
        }
    }

    fn get_attribute_name(&self, node: &AstNode) -> Option<String> {
        match node {
            AstNode::Identifier { .. } => {
                // Would need source text to get actual name
                Some("attribute".to_string())
            }
            _ => None,
        }
    }

    pub fn find_scope_at_position(&self, location: Location) -> Option<&ScopeInfo> {
        self.scopes.iter().find(|scope| {
            location.start_byte >= scope.range.start.start_byte
                && location.start_byte <= scope.range.end.start_byte
        })
    }

    pub fn get_scope_variables(&self, scope_id: usize) -> Vec<String> {
        let mut variables = Vec::new();
        let mut current_scope = Some(scope_id);

        // Collect variables from current scope and all parent scopes
        while let Some(id) = current_scope {
            if let Some(scope) = self.scopes.get(id) {
                variables.extend(scope.variables.clone());
                current_scope = scope.parent;
            } else {
                break;
            }
        }

        variables
    }
}
