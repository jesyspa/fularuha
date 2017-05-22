#[derive(Debug,Clone,Copy,Serialize,Deserialize)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Equal,
    LessThan,
    Branch,
    Print
}


#[derive(Debug,Clone,Copy,Serialize,Deserialize)]
pub enum Inst {
    PushConstant(i32),
    PushBoolConstant(bool),
    PushRelative(usize),
    PushRelativeRight(usize),
    PushJump(usize),
    MakeApp,
    Unwind,
    Slide(usize),
    GetRight,
    ExecBuiltin(Op),
    Return,
    Eval,
    EvalRelative(usize),
    DebugPrintStack,
    Terminate,
}
