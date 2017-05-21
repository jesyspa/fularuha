#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

extern crate fularuha_bytecode;
use fularuha_bytecode::defs as bytecode;

mod evaluator;
mod eval_context;
#[cfg(test)]
mod tests;

fn main() { }
