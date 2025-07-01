mod core;
mod error;
mod features;
mod server;

use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Creates a amls.toml on the current working directory or on the specified path
    Init {
        /// The path to create the amls.toml file in.
        path: Option<String>,
    },
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    if let Some(Commands::Init { path }) = &cli.command {
        println!("Initialising in {}", path.as_deref().unwrap_or("."));
        return;
    }

    server::start().await;
}
