#[derive(Debug)]
pub enum Inst {
    PushConstant(i32),
    MakeApp,
    Slide(usize),
    DebugPrintStack,
}
