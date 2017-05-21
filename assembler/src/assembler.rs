use asm::Stmt;
use bytecode::Inst;

pub fn assemble(code: &[Stmt]) -> Vec<Inst> {
    let mut result: Vec<Inst> = Vec::new();
    for a in code {
        if let Stmt::RawInst(x) = *a {
            result.push(x);
        }
    }
    result
}
