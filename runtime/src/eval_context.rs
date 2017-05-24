use std::rc::Rc;
use bytecode::{Inst, Op};

#[derive(Debug, Eq, PartialEq)]
pub enum Node<'a> {
    App(Rc<Node<'a>>, Rc<Node<'a>>),
    Num(i32),
    Bool(bool),
    Jump(usize, &'a str),
}

#[derive(Debug)]
pub enum Response<'a> {
    Terminate,
    Return(Rc<Node<'a>>),
    RequestEval(Rc<Node<'a>>),
}

pub struct EvalContext<'a> {
    stack: Vec<Rc<Node<'a>>>,
    pc: usize,
}

fn is_whnf(node: &Rc<Node>) -> bool {
    match **node {
        Node::Num(_) => true,
        Node::Bool(_) => true,
        _ => false
    }
}

impl<'a> EvalContext<'a> {
    pub fn new() -> EvalContext<'a> {
        EvalContext { stack: Vec::new(), pc: 0 }
    }

    pub fn new_from_tree(node: Rc<Node<'a>>) -> EvalContext<'a> {
        let mut ctx = EvalContext::new();
        ctx.push(node);
        ctx.unwind();
        ctx
    }

    pub fn eval(&mut self, inst: &'a Inst) -> Option<Response<'a>> {
        println!("Executing: {:?}", inst);
        match *inst {
            Inst::PushConstant(x) => { self.push_constant(x); None },
            Inst::PushBoolConstant(x) => { self.push_bool_constant(x); None },
            Inst::PushRelative(x) => { self.push_relative(x); None },
            Inst::PushRelativeRight(x) => { self.push_relative(x); self.get_right(); None },
            Inst::PushJump(x, ref s) => { self.push_jump(x, s.as_str()); None },
            Inst::MakeApp => { self.make_app(); None },
            Inst::Unwind => self.unwind(),
            Inst::Slide(x) => { self.slide(x); None },
            Inst::GetRight => { self.get_right(); None },
            Inst::ExecBuiltin(op) => { self.exec_builtin(op); None },
            Inst::Return => Some(self.return_root()),
            Inst::Eval => self.nest_eval(),
            Inst::EvalRelative(x) =>  { self.push_relative(x); self.get_right(); self.nest_eval() },
            Inst::DebugPrintStack => { self.print_stack(); None },
            Inst::Terminate => Some(Response::Terminate),
        }
    }

    pub fn run(&mut self, code: &'a [Inst]) -> Response<'a> {
        loop {
            self.print_stack();
            let pc = self.pc;
            let inst = &code[pc];
            let response = self.eval(inst);
            if self.pc == pc {
                self.pc += 1;
            }
            if let Some(r) = response {
                return r
            }
        }
    }

    fn push_constant(&mut self, c: i32) {
        self.push(Rc::new(Node::Num(c)));
    }

    fn push_bool_constant(&mut self, c: bool) {
        self.push(Rc::new(Node::Bool(c)));
    }

    fn push_relative(&mut self, n: usize) {
        let m = self.stack.len();
        let copy = self.stack[m - 1 - n].clone();
        self.push(copy);
    }

    fn push_jump(&mut self, n: usize, s: &'a str) {
        self.push(Rc::new(Node::Jump(n, s)));
    }

    fn make_app(&mut self) {
        let left = self.pop();
        let right = self.pop();
        self.push(Rc::new(Node::App(left, right)));
    }

    fn unwind(&mut self) -> Option<Response<'a>> {
        if is_whnf(self.top()) {
            let node = self.pop();
            assert!(self.stack.is_empty());
            Some(Response::Return(node))
        } else {
            self.unwind_rec();
            None
        }
    }

    fn unwind_rec(&mut self) {
        enum UnwindResult<'a> {
            Recurse(Rc<Node<'a>>),
            Jump(usize, &'a str),
            Die,
        }
        let result = {
            let top: &Node = self.stack.last().expect("stack underflow");
            match *top {
                Node::App(ref left, _) => UnwindResult::Recurse(left.clone()),
                Node::Jump(target, s) =>  UnwindResult::Jump(target, s),
                _ => UnwindResult::Die,
            }
        };
        match result {
            UnwindResult::Recurse(x) => { self.push(x); self.unwind(); },
            UnwindResult::Jump(target, s) => { println!("Jumping to {}", s); self.pc = target; },
            UnwindResult::Die => panic!("invalid application"),
        };
    }

    fn slide(&mut self, n: usize) {
        let top = self.pop();
        let m = self.stack.len();
        assert!(m >= n, "sliding too far");
        self.stack.truncate(m - n);
        self.push(top);
        // TODO: Update the app on top.
    }

    fn get_right(&mut self) {
        let top = self.pop();
        let right = match *top {
            Node::App(_, ref right) => Some(right.clone()),
            _ => None
        }.expect("expected app");
        self.push(right);
    }

    fn exec_builtin(&mut self, op: Op) {
        match op {
            Op::Add => {
                let a = self.pop_num();
                let b = self.pop_num();
                self.push(Rc::new(Node::Num(a+b)));
            }
            Op::Sub => {
                let a = self.pop_num();
                let b = self.pop_num();
                self.push(Rc::new(Node::Num(a-b)));
            }
            Op::Mul => {
                let a = self.pop_num();
                let b = self.pop_num();
                self.push(Rc::new(Node::Num(a*b)));
            }
            Op::Equal => {
                let a = self.pop_num();
                let b = self.pop_num();
                self.push(Rc::new(Node::Bool(a == b)));
            }
            Op::LessThan => {
                let a = self.pop_num();
                let b = self.pop_num();
                self.push(Rc::new(Node::Bool(a < b)));
            }
            Op::Branch => {
                let c = self.pop_bool();
                let if_true = self.pop();
                let if_false = self.pop();
                self.push(if c { if_true } else { if_false });
            }
            Op::Print => {
                let a = self.pop_num();
                println!("{}", a);
            }
        };
    }

    fn return_root(&mut self) -> Response<'a> {
        assert_eq!(self.stack.len(), 1);
        Response::Return(self.pop())
    }

    fn nest_eval(&mut self) -> Option<Response<'a>> {
        if !is_whnf(self.top()) {
            Some(Response::RequestEval(self.pop()))
        } else {
            println!("Node already in normal form.");
            None
        }
    }

    fn print_stack(&self) {
        println!("Vector contents:");
        for (i, n) in self.stack.iter().enumerate() {
            match **n {
                Node::App(ref left, ref right) =>
                    if i + 1 != self.stack.len() && self.stack[i+1] == *left {
                        println!("-> App(..., {:?})", right);
                    } else {
                        println!("-> {:?}", n);
                    },
                _ => println!("-> {:?}", n)
            }
        }
    }

    pub fn push(&mut self, node: Rc<Node<'a>>) {
        self.stack.push(node);
    }

    fn top(&self) -> &Rc<Node<'a>> {
        self.stack.last().expect("stack underflow")
    }

    fn pop(&mut self) -> Rc<Node<'a>> {
        self.stack.pop().expect("stack underflow")
    }

    fn pop_num(&mut self) -> i32 {
        if let Node::Num(x) = *self.pop() {
            x
        } else {
            panic!("expected number");
        }
    }

    fn pop_bool(&mut self) -> bool {
        if let Node::Bool(x) = *self.pop() {
            x
        } else {
            panic!("expected number");
        }
    }

}
