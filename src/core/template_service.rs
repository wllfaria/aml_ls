use std::collections::HashMap;
use std::path::PathBuf;

use aml_config::Config;
use aml_core::workspace::get_root_template;
use aml_semantic::global_scope::{GlobalScope, GlobalSymbol};
use aml_semantic::{SemanticAnalyzer, SymbolType};
use aml_syntax::ast::*;
use tower_lsp::lsp_types::*;

use crate::core::document_manager::{DocumentManager, parse_content};

#[derive(Debug)]
pub struct Template {
    pub path: PathBuf,
}

#[derive(Debug, Default)]
pub struct Templates {
    inner: Vec<Template>,
    name_map: HashMap<String, usize>,
    path_map: HashMap<PathBuf, usize>,
}

impl Templates {
    pub fn register(&mut self, name: String, path: PathBuf, _: bool) {
        let index = self.inner.len();
        let template = Template { path: path.clone() };

        self.name_map.insert(name, index);
        self.path_map.insert(path, index);
        self.inner.push(template);
    }

    pub fn has_template_by_name(&self, name: &str) -> bool {
        self.name_map.contains_key(name)
    }

    pub fn paths(&self) -> impl Iterator<Item = &Template> {
        self.inner.iter()
    }
}

#[derive(Debug, Default)]
pub struct TemplateService {
    templates: Templates,
}

impl TemplateService {
    pub fn new() -> Self {
        Self::default()
    }

    /// Discovers all templates starting from the root template.
    /// Returns the templates registry and any discovered global scope symbols.
    pub fn discover_templates(
        &mut self,
        root_dir: &PathBuf,
        config: &Config,
        global_scope: &mut GlobalScope,
        document_manager: &mut DocumentManager,
    ) -> Result<(), TemplateDiscoveryError> {
        let root_template_path = get_root_template(root_dir, config)
            .ok_or(TemplateDiscoveryError::RootTemplateNotFound)?;

        let content = std::fs::read_to_string(&root_template_path)
            .map_err(|e| TemplateDiscoveryError::FileReadError(root_template_path.clone(), e))?;

        let ast = parse_content(&content);
        self.templates
            .register("index".into(), root_template_path.clone(), false);

        ast.accept(&mut TemplateCollector {
            config,
            content: &content,
            root_dir,
            templates: &mut self.templates,
            file_path: &root_template_path,
            global_scope,
            document_manager,
        });

        Ok(())
    }

    /// Preloads all discovered templates for workspace-wide analysis.
    pub async fn preload_templates(
        &self,
        global_scope: &mut GlobalScope,
        document_manager: &mut DocumentManager,
    ) -> Result<(), TemplateDiscoveryError> {
        for template in self.templates.paths() {
            let content = std::fs::read_to_string(&template.path)
                .map_err(|e| TemplateDiscoveryError::FileReadError(template.path.clone(), e))?;

            let uri = Url::from_file_path(&template.path)
                .map_err(|_| TemplateDiscoveryError::InvalidPath(template.path.clone()))?;

            let ast = parse_content(&content);
            document_manager
                .add_or_update_file(global_scope, uri, ast, content, 0)
                .await;
        }

        Ok(())
    }

    pub fn templates_mut(&mut self) -> &mut Templates {
        &mut self.templates
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TemplateDiscoveryError {
    #[error("Root template not found in workspace")]
    RootTemplateNotFound,
    #[error("Failed to read file {0}: {1}")]
    FileReadError(PathBuf, std::io::Error),
    #[error("Invalid file path: {0}")]
    InvalidPath(PathBuf),
}

/// Discovers and registers templates during workspace initialization by walking the AST of
/// templates, collecting:
///
/// - Global variable declarations (registered in `GlobalScope`)
/// - Component references (recursively processes referenced template files)
struct TemplateCollector<'src> {
    content: &'src str,
    config: &'src Config,
    root_dir: &'src PathBuf,
    file_path: &'src PathBuf,
    templates: &'src mut Templates,
    global_scope: &'src mut GlobalScope,
    document_manager: &'src mut DocumentManager,
}

impl<'src> AstVisitor<'src> for TemplateCollector<'src> {
    fn visit_globals(&mut self, decl: &Declaration, _: &AstNode) {
        let name = decl.name.text(self.content).into();
        let mut analyzer = SemanticAnalyzer::new(self.content, self.global_scope);
        let symbol_type = SymbolType::Variable(analyzer.analyze_expression(&decl.value));

        self.global_scope.declare_global(GlobalSymbol {
            name,
            symbol_type,
            location: decl.location,
            definition: self.file_path.clone(),
        });
    }

    fn visit_component(&mut self, component: &Component, _: &AstNode) {
        let name = component.name.text(self.content);
        let path = PathBuf::from(name).with_extension("aml");
        let file_path = self.root_dir.join(&self.config.templates_dir).join(&path);

        // Prevent infinite recursion by checking if template is already registered.
        // TODO(wiru): this has to become a diagnostic of cyclic dependency
        if self.templates.has_template_by_name(name) {
            return;
        }

        self.templates
            .register(name.into(), file_path.clone(), false);
        let Ok(content) = std::fs::read_to_string(&file_path) else { return };
        let tokens = aml_token::Lexer::new(&content).collect();
        let tokens = aml_token::Tokens::new(tokens, content.len());
        let ast = aml_syntax::Parser::new(tokens).parse();

        ast.accept(&mut TemplateCollector {
            content: &content,
            config: self.config,
            file_path: &file_path,
            root_dir: self.root_dir,
            templates: self.templates,
            global_scope: self.global_scope,
            document_manager: self.document_manager,
        });
    }

    fn visit_container(&mut self, container: &'src ContainerNode, _: &'src AstNode) {
        for child in container.children.iter() {
            child.accept(self);
        }
    }

    fn visit_text(&mut self, text: &'src Text, _: &'src AstNode) {
        for child in text.children.iter() {
            child.accept(self);
        }
    }

    fn visit_for(&mut self, for_loop: &'src For, _: &'src AstNode) {
        for child in for_loop.children.iter() {
            child.accept(self);
        }
    }
}

