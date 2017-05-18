#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

mod evaluator;
use evaluator::evaluate;
mod eval_context;
mod bytecode;
use bytecode::{Inst, Op};

fn main() {
    let code = vec![
        Inst::PushConstant(2),
        Inst::PushConstant(3),
        Inst::PushJump(8),
        Inst::MakeApp,
        Inst::MakeApp,
        Inst::PushJump(26),
        Inst::MakeApp,
        Inst::Unwind,
        // 8: Add
        Inst::PushRelative(1),
        Inst::GetRight,
        Inst::Eval,
        Inst::PushRelative(3),
        Inst::GetRight,
        Inst::Eval,
        Inst::ExecBuiltin(Op::Add),
        Inst::Slide(3),
        Inst::Return,
        // 17: Mul
        Inst::PushRelative(1),
        Inst::GetRight,
        Inst::Eval,
        Inst::PushRelative(3),
        Inst::GetRight,
        Inst::Eval,
        Inst::ExecBuiltin(Op::Mul),
        Inst::Slide(3),
        Inst::Return,
        // 26: f x = x * x
        Inst::PushRelative(1),
        Inst::GetRight,
        Inst::PushRelative(2),
        Inst::GetRight,
        Inst::PushJump(17),
        Inst::MakeApp,
        Inst::MakeApp,
        Inst::Slide(2),
        Inst::Unwind,
    ];

    let code_simplified = vec![
        Inst::PushConstant(2),
        Inst::PushConstant(3),
        Inst::PushJump(13),
        Inst::MakeApp,
        Inst::MakeApp,
        Inst::PushJump(8),
        Inst::MakeApp,
        Inst::Unwind,
        // 8: f x = x * x
        Inst::EvalRelative(1),
        Inst::PushRelative(0),
        Inst::ExecBuiltin(Op::Mul),
        Inst::Slide(2),
        Inst::Return,
        // 13: + (optimised)
        Inst::PushRelativeRight(1),
        Inst::PushRelativeRight(3),
        Inst::ExecBuiltin(Op::Add),
        Inst::Slide(3),
        Inst::Return,
    ];
    evaluate(&code);
    evaluate(&code_simplified);
}
