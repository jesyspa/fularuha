#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

extern crate fularuha_bytecode;
use fularuha_bytecode::defs as bytecode;

mod evaluator;
mod eval_context;
mod debug_log;
use debug_log::DebugLog;
#[cfg(test)]
mod tests;

use std::fs::File;
use std::io::Read;
extern crate serde_json;

extern crate clap;
use clap::{Arg, App};

fn main() {
    let matches = App::new("FuLaRuHa runtime")
        .arg(Arg::with_name("INPUT")
             .help("File to run")
             .required(true)
             .index(1))
        .arg(Arg::with_name("verbosity")
             .long("verbose")
             .short("v")
             .multiple(true))
        .get_matches();


    let filename = matches.value_of("INPUT").expect("no file provided");
    let mut file = File::open(filename).expect("invalid file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("read failed");
    let code: Vec<bytecode::Inst> = serde_json::from_str(&contents).expect("decode failed");
    let log = DebugLog::new(matches.occurrences_of("verbosity"));
    evaluator::evaluate(&code, &log);
}
