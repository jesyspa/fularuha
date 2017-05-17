#[derive(Debug,Clone,Copy)]
#[allow(dead_code)] // We'll get around to it.
pub enum Op {
    Add,
    Sub,
    Mul,
    Equal,
    LessThan,
    Branch
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
    Return,
    Eval,
    DebugPrintStack,
    Terminate,
}
