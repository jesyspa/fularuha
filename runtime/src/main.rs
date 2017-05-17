#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

mod eval_context;
use eval_context::EvalContext;
mod bytecode;
use bytecode::{Inst, Op};

fn main() {
    let code = vec![
        Inst::PushConstant(9),
        Inst::PushConstant(5),
        Inst::PushJump(8),
        Inst::DebugPrintStack,
        Inst::MakeApp,
        Inst::MakeApp,
        Inst::DebugPrintStack,
        Inst::Unwind,
        // Execution jumps elsewhere
        Inst::DebugPrintStack,
        Inst::PushRelative(0),
        Inst::GetRight,
        Inst::PushRelative(2),
        Inst::GetRight,
        Inst::DebugPrintStack,
        Inst::ExecBuiltin(Op::Add),
        Inst::DebugPrintStack,
        Inst::Slide(2),
        Inst::DebugPrintStack,
        Inst::Terminate,
    ];
    let mut ec = EvalContext::new(&code);

    ec.run();
}
