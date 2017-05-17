#[derive(Debug,Clone,Copy)]
pub enum Op {
    Add,
}


#[derive(Debug,Clone,Copy)]
pub enum Inst {
    PushConstant(i32),
    PushRelative(usize),
    PushJump(usize),
    MakeApp,
    Unwind,
    Slide(usize),
    GetRight,
    ExecBuiltin(Op),
    DebugPrintStack,
    Terminate,
}
