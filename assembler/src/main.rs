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

use std::fs::File;
use std::io::Read;
use std::io::Write;

extern crate serde_json;

extern crate clap;
use clap::{Arg, App};

fn main() {
    let matches = App::new("FuLaRuHa assembler")
        .arg(Arg::with_name("INPUT")
             .help("File to compile")
             .required(true)
             .index(1))
        .arg(Arg::with_name("output")
             .long("output")
             .short("o")
             .value_name("FILE")
             .takes_value(true))
        .get_matches();

    let infile_name = matches.value_of("INPUT").expect("no input provided");
    let mut infile = File::open(infile_name).expect("invalid file");
    let mut contents = String::new();
    infile.read_to_string(&mut contents).expect("read failed");
    let parsed_input = parse(&contents);

    let assembled_output = assemble(&parsed_input);

    let outfile_name = matches.value_of("output").unwrap_or("output.json");
    let mut outfile = File::create(outfile_name).expect("failed to create file");
    let json = serde_json::to_string(&assembled_output).expect("failed to encode");
    outfile.write(json.as_bytes()).expect("write failed");
}

