#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

extern crate fularuha_bytecode;
use fularuha_bytecode::defs as bytecode;

mod evaluator;
mod eval_context;
#[cfg(test)]
mod tests;

use std::env;
use std::fs::File;
use std::io::Read;
extern crate serde_json;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Please provide a file to run");
    } else {
        let mut file = File::open(&args[1]).expect("invalid file");
        let mut contents = String::new();
        file.read_to_string(&mut contents).expect("read failed");
        let code: Vec<bytecode::Inst> = serde_json::from_str(&contents).expect("decode failed");
        evaluator::evaluate(&code);
    }
}
