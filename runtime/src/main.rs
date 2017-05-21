#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

mod evaluator;
use evaluator::evaluate;
mod eval_context;
mod bytecode;
use bytecode::{Inst, Op};
#[cfg(test)]
mod tests;

fn main() { }
