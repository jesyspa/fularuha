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
            Inst::Slide(x) => self.slide(x),
            Inst::DebugPrintStack => self.print_stack(),
        }
    }

    fn push_constant(&mut self, c: i32) {
        self.stack.push(Box::new(Node::Num(c)));
    }

    fn make_app(&mut self) {
        let right = self.pop();
        let left = self.pop();
        self.stack.push(Box::new(Node::App(left, right)));
    }

    fn slide(&mut self, n: usize) {
        let top = self.pop();
        let m = self.stack.len();
        // NOTE: We want to error out if n is too big.
        self.stack.truncate(m - n);
        self.stack.push(top);
        // TODO: Update the app on top.
    }

    fn print_stack(&self) {
        println!("Vector contents:");
        for n in &self.stack {
            println!("-> {:?}", n);
        }
    }

    fn pop(&mut self) -> Box<Node> {
        self.stack.pop().expect("stack underflow")
    }
}
