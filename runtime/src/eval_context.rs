use std::rc::Rc;
use bytecode::{Inst, Op};

#[derive(Debug)]
enum Node {
    App(Rc<Node>, Rc<Node>),
    Num(i32),
    Jump(usize),
}

pub struct EvalContext<'a> {
    stack: Vec<Rc<Node>>,
    code: &'a [Inst],
    pc: usize,
    running: bool,
}

impl<'a> EvalContext<'a> {
    pub fn new(code: &[Inst]) -> EvalContext {
        EvalContext { stack: Vec::new(), code, pc: 0, running: false, }
    }

    pub fn eval(&mut self, inst: Inst) {
        match inst {
            Inst::PushConstant(x) => self.push_constant(x),
            Inst::PushRelative(x) => self.push_relative(x),
            Inst::PushJump(x) => self.push_jump(x),
            Inst::MakeApp => self.make_app(),
            Inst::Unwind => self.unwind(),
            Inst::Slide(x) => self.slide(x),
            Inst::GetRight => self.get_right(),
            Inst::ExecBuiltin(op) => self.exec_builtin(op),
            Inst::DebugPrintStack => self.print_stack(),
            Inst::Terminate => self.terminate(),
        }
    }

    pub fn run(&mut self) {
        self.running = true;
        while self.running {
            let pc = self.pc;
            let inst = self.code[pc];
            self.eval(inst);
            if self.pc == pc {
                self.pc += 1;
            }
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

    fn push_jump(&mut self, n: usize) {
        self.stack.push(Rc::new(Node::Jump(n)));
    }

    fn make_app(&mut self) {
        let left = self.pop();
        let right = self.pop();
        self.stack.push(Rc::new(Node::App(left, right)));
    }

    fn unwind(&mut self) {
        enum UnwindResult {
            Recurse(Rc<Node>),
            Jump(usize),
            Die,
        }
        let result = {
            let top: &Node = self.stack.last().expect("stack underflow");
            match *top {
                Node::App(ref left, _) => UnwindResult::Recurse(left.clone()),
                Node::Jump(target) => UnwindResult::Jump(target),
                _ => UnwindResult::Die,
            }
        };
        match result {
            UnwindResult::Recurse(x) => { self.stack.push(x); self.unwind(); },
            UnwindResult::Jump(target) => { self.stack.pop(); self.pc = target; },
            UnwindResult::Die => panic!("invalid application"),
        };
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

    fn exec_builtin(&mut self, op: Op) {
        match op {
            Op::Add => {
                let a = match *self.pop() {
                    Node::Num(x) => Some(x),
                    _ => None,
                }.expect("adding non-number");
                let b = match *self.pop() {
                    Node::Num(x) => Some(x),
                    _ => None,
                }.expect("adding non-number");
                let total = Rc::new(Node::Num(a+b));
                self.stack.push(total);
            }
        };
    }

    fn print_stack(&self) {
        println!("Vector contents:");
        for n in &self.stack {
            println!("-> {:?}", n);
        }
    }

    fn terminate(&mut self) {
        self.running = false;
    }

    fn pop(&mut self) -> Rc<Node> {
        self.stack.pop().expect("stack underflow")
    }
}
