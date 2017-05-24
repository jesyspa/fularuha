use eval_context::*;
use bytecode::Inst;

pub struct DebugLog {
    verbosity: u64,
}

impl DebugLog {
    pub fn new(verbosity: u64) -> DebugLog {
        DebugLog { verbosity }
    }

    pub fn print_stack(&self, ec: &EvalContext, forced: bool) {
        if !forced && self.verbosity <= 1 {
            return;
        }
        ec.print_stack();
    }

    pub fn print_response(&self, response: &Response) {
        if self.verbosity <= 0 {
            return;
        }
        match *response {
            Response::Terminate => println!("Terminating"),
            Response::Return(ref node) => println!("Returning: {:?}", node),
            Response::RequestEval(ref node) => println!("Evaluating: {:?}", node),
        };
    }

    pub fn print_inst(&self, inst: &Inst) {
        if self.verbosity <= 1 {
            return;
        }
        println!("Executing: {:?}", inst);
    }

    pub fn print_eval_unnecessary(&self) {
        if self.verbosity > 1 {
            println!("Node already in normal form.");
        }
    }

    pub fn print_jump(&self, target: &str) {
        if self.verbosity > 0 {
            println!("Jumping to {}", target);
        }
    }
}
