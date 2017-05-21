use asm::Stmt;
use bytecode::Inst;

use std::collections::HashMap;

pub fn assemble(code: &[Stmt]) -> Vec<Inst> {
    let mut result: Vec<Inst> = Vec::new();
    let mut labels: HashMap<&str, usize> = HashMap::new();
    let mut ix = 0;
    for stmt in code {
        if let Stmt::Label(ref x) = *stmt {
            labels.insert(x.as_str(), ix);
        } else {
            ix += 1;
        }
    }
    for stmt in code {
        match *stmt {
            Stmt::RawInst(ref x) => { result.push(*x); },
            Stmt::PushLabelJump(ref x) => {
                let ix = labels[x.as_str()];
                result.push(Inst::PushJump(ix));
            },
            Stmt::Label(_) => ()
        };
    }
    result
}
