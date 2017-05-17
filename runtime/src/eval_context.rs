use bytecode::Inst;

#[derive(Debug)]
enum Node {
    App(Box<Node>, Box<Node>),
    Num(i32),
}

pub struct EvalContext {
    stack: Vec<Box<Node>>,
}

impl EvalContext {
    pub fn new() -> EvalContext {
        EvalContext { stack: Vec::new() }
    }

    pub fn eval(&mut self, inst: Inst) {
        match inst {
            Inst::PushConstant(x) => self.push_constant(x),
            Inst::MakeApp => self.make_app(),
            Inst::DebugPrintStack => self.print_stack(),
        }
    }

    fn push_constant(&mut self, c: i32) {
        self.stack.push(Box::new(Node::Num(c)));
    }

    fn make_app(&mut self) {
        let right = self.stack.pop().expect("stack underflow");
        let left = self.stack.pop().expect("stack underflow");
        self.stack.push(Box::new(Node::App(left, right)));
    }

    fn print_stack(&self) {
        println!("Vector contents:");
        for n in &self.stack {
            println!("{:?}", n);
        }
    }
}
