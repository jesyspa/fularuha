#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]
#![feature(plugin)]
#![plugin(peg_syntax_ext)]


extern crate fularuha_bytecode;
use fularuha_bytecode::defs as bytecode;

mod asm;
mod parser;
mod assembler;

use parser::parse;
use assembler::assemble;

use std::env;
use std::fs::File;
use std::io::Read;
use std::io::Write;
extern crate serde_json;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Please provide a file to assemble");
    } else {
        let mut infile = File::open(&args[1]).expect("invalid file");
        let mut contents = String::new();
        infile.read_to_string(&mut contents).expect("read failed");
        let parsed_input = parse(&contents);

        let assembled_output = assemble(&parsed_input);

        let mut outfile = File::create("output.json").expect("failed to open file");
        let json = serde_json::to_string(&assembled_output).expect("failed to encode");
        outfile.write(json.as_bytes()).expect("write failed");
    }
}

