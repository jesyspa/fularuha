use std::io;
mod eval_context;
use eval_context::EvalContext;
mod bytecode;
use bytecode::Inst;

fn main() {
    let mut ec = EvalContext::new();

    loop {
        let mut cmd = String::new();
        io::stdin().read_line(&mut cmd)
            .expect("read error");

        let cmd = cmd.trim();

        let inst : Inst;

        if cmd == "MkApp" {
            inst = Inst::MakeApp;
        } else if cmd.starts_with("PushConstant") {
            let cstr = cmd.split(' ').nth(1).expect("constant expected");
            inst = Inst::PushConstant(cstr.parse().expect("constant not numeric"));
        } else if cmd.starts_with("PushRelative") {
            let cstr = cmd.split(' ').nth(1).expect("constant expected");
            inst = Inst::PushRelative(cstr.parse().expect("constant not numeric"));
        } else if cmd == "Unwind" {
            inst = Inst::Unwind;
        } else if cmd == "Print" {
            inst = Inst::DebugPrintStack;
        } else if cmd.starts_with("Slide") {
            let cstr = cmd.split(' ').nth(1).expect("constant expected");
            inst = Inst::Slide(cstr.parse().expect("constant not numeric"));
        } else if cmd == "GetRight" {
            inst = Inst::GetRight;
        } else if cmd == "Exit" {
            println!("Bye!");
            return;
        } else {
            println!("Unknown command: {}", cmd);
            continue;
        }
        ec.eval(inst);
    }
}
