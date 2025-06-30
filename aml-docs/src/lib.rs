#[derive(Debug)]
pub struct Docs {
    pub text: &'static str,
    pub span: &'static str,
    pub border: &'static str,
    pub alignment: &'static str,
    pub vstack: &'static str,
    pub hstack: &'static str,
    pub zstack: &'static str,
    pub row: &'static str,
    pub column: &'static str,
    pub expand: &'static str,
    pub position: &'static str,
    pub spacer: &'static str,
    pub overflow: &'static str,
    pub padding: &'static str,
    pub canvas: &'static str,
    pub container: &'static str,
}

impl Default for Docs {
    fn default() -> Self {
        Self::new()
    }
}

impl Docs {
    pub fn new() -> Self {
        Self {
            text: include_str!("../../docs/elements/text.md"),
            span: include_str!("../../docs/elements/span.md"),
            border: include_str!("../../docs/elements/border.md"),
            alignment: include_str!("../../docs/elements/alignment.md"),
            vstack: include_str!("../../docs/elements/vstack.md"),
            hstack: include_str!("../../docs/elements/hstack.md"),
            zstack: include_str!("../../docs/elements/zstack.md"),
            row: include_str!("../../docs/elements/row.md"),
            column: include_str!("../../docs/elements/column.md"),
            expand: include_str!("../../docs/elements/expand.md"),
            position: include_str!("../../docs/elements/position.md"),
            spacer: include_str!("../../docs/elements/spacer.md"),
            overflow: include_str!("../../docs/elements/overflow.md"),
            padding: include_str!("../../docs/elements/padding.md"),
            canvas: include_str!("../../docs/elements/canvas.md"),
            container: include_str!("../../docs/elements/container.md"),
        }
    }
}

pub fn lib() {}
