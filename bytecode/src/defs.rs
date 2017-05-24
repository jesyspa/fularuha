#[derive(Debug,Clone,Copy,Serialize,Deserialize)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Equal,
    LessThan,
    Branch,
    Switch(usize),
    Print
}


#[derive(Debug,Clone,Serialize,Deserialize)]
pub enum Inst {
    PushConstant(i32),
    PushBoolConstant(bool),
    PushRelative(usize),
    PushJump(usize, String),
    PushArg(usize),
    PushArgStrict(usize),
    // Constructor, number of args
    MemAlloc(usize, usize),
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
