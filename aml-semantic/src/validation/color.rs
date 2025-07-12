use aml_token::Hex;

pub fn is_valid_color_name<S: AsRef<str>>(name: S) -> bool {
    let name = name.as_ref().to_lowercase();
    matches!(
        name.as_str(),
        "reset"
            | "black"
            | "red"
            | "green"
            | "yellow"
            | "blue"
            | "magenta"
            | "cyan"
            | "grey"
            | "dark_grey"
            | "light_red"
            | "light_green"
            | "light_yellow"
            | "light_blue"
            | "light_magenta"
            | "light_cyan"
            | "white"
    )
}

pub fn is_valid_ansi_color<S: AsRef<str>>(str: S) -> bool {
    str.as_ref().parse::<u8>().is_ok()
}

pub fn is_valid_hex_color<S: AsRef<str>>(str: S) -> bool {
    Hex::try_from(str.as_ref()).is_ok()
}

pub fn is_valid_color<S: AsRef<str>>(str: S) -> bool {
    is_valid_color_name(str.as_ref())
        || is_valid_ansi_color(str.as_ref())
        || is_valid_hex_color(str.as_ref())
}
