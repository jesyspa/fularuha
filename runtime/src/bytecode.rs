#[derive(Debug)]
pub enum Inst {
    PushConstant(i32),
    PushRelative(usize),
    MakeApp,
    Unwind,
    Slide(usize),
    GetRight,
    DebugPrintStack,
}
