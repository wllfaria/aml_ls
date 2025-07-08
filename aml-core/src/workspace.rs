use aml_config::Config;
use std::path::{Path, PathBuf};

pub fn get_root_template<P: AsRef<Path>>(root_dir: P, config: &Config) -> Option<PathBuf> {
    let root_template = root_dir
        .as_ref()
        .join(&config.templates_dir)
        .join(&config.root_template);

    if !root_template.exists() {
        // TOOD(wiru) figure out what to do if the main template doesn't exist
        return None;
    }

    Some(root_template)
}

pub fn search_for_templates<P: AsRef<Path>>(path: P) -> Vec<PathBuf> {
    let mut templates = vec![];
    search_for_templates_inner(path, &mut templates);
    templates
}

fn search_for_templates_inner<P: AsRef<Path>>(path: P, templates: &mut Vec<PathBuf>) {
    let Ok(files) = std::fs::read_dir(path) else { return };

    for file in files.flatten() {
        let Ok(file_type) = file.file_type() else { continue };

        if file_type.is_dir() {
            search_for_templates_inner(file.path(), templates);
        }

        if file_type.is_file() {
            let path = file.path();

            if !path.extension().map(|ext| ext == "aml").unwrap_or_default() {
                continue;
            };

            templates.push(path);
        }
    }
}
