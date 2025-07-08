use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub struct Config {
    pub templates_dir: PathBuf,
    pub root_template: String,
}

impl From<RawConfig> for Config {
    fn from(raw_config: RawConfig) -> Self {
        Self {
            templates_dir: raw_config
                .templates_dir
                .unwrap_or("templates".into())
                .into(),
            root_template: raw_config.root_template.unwrap_or("index.aml".into()),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct RawConfig {
    templates_dir: Option<String>,
    root_template: Option<String>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            templates_dir: "templates".into(),
            root_template: "index.aml".into(),
        }
    }
}

fn load_project_config<P: AsRef<Path>>(root_dir: P) -> Option<Config> {
    let config_path = root_dir.as_ref().join("aml_ls.toml");
    if !config_path.exists() {
        return None;
    };

    let config = std::fs::read_to_string(config_path).ok()?;

    convert_from_toml(&config)
}

fn convert_from_toml(config: &str) -> Option<Config> {
    let raw_config: RawConfig = toml::from_str(config).ok()?;
    Some(raw_config.into())
}

pub fn load_config<P: AsRef<Path>>(root_dir: Option<P>) -> Config {
    match root_dir {
        Some(root_dir) => load_project_config(root_dir).unwrap_or_default(),
        None => Config::default(),
    }
}
