use std::rc::Rc;
use bytecode::Inst;

#[derive(Debug)]
enum Node {
    App(Rc<Node>, Rc<Node>),
    Num(i32),
}

pub struct EvalContext {
    stack: Vec<Rc<Node>>,
}

impl EvalContext {
    pub fn new() -> EvalContext {
        EvalContext { stack: Vec::new() }
    }

    pub fn eval(&mut self, inst: Inst) {
        match inst {
            Inst::PushConstant(x) => self.push_constant(x),
            Inst::PushRelative(x) => self.push_relative(x),
            Inst::MakeApp => self.make_app(),
            Inst::Slide(x) => self.slide(x),
            Inst::GetRight => self.get_right(),
            Inst::DebugPrintStack => self.print_stack(),
        }
    }

    fn push_constant(&mut self, c: i32) {
        self.stack.push(Rc::new(Node::Num(c)));
    }

    fn push_relative(&mut self, n: usize) {
        let m = self.stack.len();
        let copy = self.stack[m - 1 - n].clone();
        self.stack.push(copy);
    }

    fn make_app(&mut self) {
        let right = self.pop();
        let left = self.pop();
        self.stack.push(Rc::new(Node::App(left, right)));
    }

    fn slide(&mut self, n: usize) {
        let top = self.pop();
        let m = self.stack.len();
        // NOTE: We want to error out if n is too big.
        self.stack.truncate(m - n);
        self.stack.push(top);
        // TODO: Update the app on top.
    }

    fn get_right(&mut self) {
        let top = self.pop();
        let right = match *top {
            Node::App(_, ref right) => Some(right.clone()),
            _ => None
        }.expect("expected app");
        self.stack.push(right);
    }

    fn print_stack(&self) {
        println!("Vector contents:");
        for n in &self.stack {
            println!("-> {:?}", n);
        }
    }

    fn pop(&mut self) -> Rc<Node> {
        self.stack.pop().expect("stack underflow")
    }
}
