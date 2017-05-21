use bytecode::*;
use eval_context::*;
use evaluator::*;

#[test]
fn trivial() {
    evaluate(&vec![
        Inst::Terminate
    ]);
}

#[test]
fn simple_prog() {
    let node = evaluate(&vec![
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
    ]);
    if let Some(n) = node {
        assert_eq!(*n, Node::Num(25));
    } else {
        assert!(false);
    }
}

#[test]
fn optimised_prog() {
    let node = evaluate(&vec![
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
    ]);
    if let Some(n) = node {
        assert_eq!(*n, Node::Num(25));
    } else {
        assert!(false);
    }
}
