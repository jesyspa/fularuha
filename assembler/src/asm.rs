use bytecode::Inst;

#[derive(Debug,Clone)]
pub enum Stmt {
    RawInst(Inst),
    Label(String),
    PushLabelJump(String)
}
