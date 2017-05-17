#[derive(Debug)]
pub enum Inst {
    PushConstant(i32),
    PushRelative(usize),
    MakeApp,
    Slide(usize),
    DebugPrintStack,
}
