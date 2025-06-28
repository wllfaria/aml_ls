default:
    just --list

@all:
    just build-release
    mv ./target/release/aml_ls ~/.local/bin/aml_ls

[group('build')]
@build:
    cargo build

[group('build')]
@build-release:
    cargo build --release
