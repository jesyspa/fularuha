use std::rc::Rc;
use bytecode::{Inst, Op};
use debug_log::DebugLog;

#[derive(Debug)]
pub enum Node<'a> {
    App(Rc<Node<'a>>, Rc<Node<'a>>),
    Num(i32),
    Bool(bool),
    // Target and a debug string
    Jump(usize, &'a str),
    // Constructor index and list of members.
    Struct(usize, Vec<Rc<Node<'a>>>),
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
    log: &'a DebugLog,
}

fn is_whnf(node: &Rc<Node>) -> bool {
    match **node {
        Node::Num(_) | Node::Bool(_) | Node::Struct(_, _) => true,
        _ => false
    }
}

impl<'a> EvalContext<'a> {
    pub fn new(log: &'a DebugLog) -> EvalContext<'a> {
        EvalContext { stack: Vec::new(), pc: 0, log }
    }

    pub fn new_from_tree(log: &'a DebugLog, node: Rc<Node<'a>>) -> EvalContext<'a> {
        let mut ctx = EvalContext::new(log);
        ctx.push(node);
        ctx.unwind();
        ctx
    }

    pub fn eval(&mut self, inst: &'a Inst) -> Option<Response<'a>> {
        self.log.print_inst(inst);
        match *inst {
            Inst::PushConstant(x) => { self.push_constant(x); None },
            Inst::PushBoolConstant(x) => { self.push_bool_constant(x); None },
            Inst::PushRelative(x) => { self.push_relative(x); None },
            Inst::PushJump(x, ref s) => { self.push_jump(x, s.as_str()); None },
            Inst::PushArg(x) => { self.push_relative(x); self.get_right(); None },
            Inst::PushArgStrict(x) => { self.push_relative(x); self.get_right(); self.nest_eval() },
            Inst::MemAlloc(con, size) => { self.mem_alloc(con, size); None },
            Inst::MakeApp => { self.make_app(); None },
            Inst::Unwind => self.unwind(),
            Inst::Slide(x) => { self.slide(x); None },
            Inst::GetRight => { self.get_right(); None },
            Inst::ExecBuiltin(op) => { self.exec_builtin(op); None },
            Inst::Return => Some(self.return_root()),
            Inst::Eval => self.nest_eval(),
            Inst::DebugPrintStack => { self.log.print_stack(self, true); None },
            Inst::Terminate => Some(Response::Terminate),
        }
    }

    pub fn run(&mut self, code: &'a [Inst]) -> Response<'a> {
        loop {
            self.log.print_stack(self, false);
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

    fn mem_alloc(&mut self, con: usize, size: usize) {
        let mut mems: Vec<Rc<Node<'a>>> = Vec::new();
        for _ in 0..size {
            mems.push(self.pop());
        }
        self.push(Rc::new(Node::Struct(con, mems)));
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
            UnwindResult::Jump(target, s) => { self.log.print_jump(s); self.pc = target; },
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
            Op::Switch(n) => {
                if let Node::Struct(con, ref data) = *self.pop() {
                    // This feels a bit too complicated for one instruction, though partially
                    // that's just due to how it's expressed in Rust.  Clean it up sometime?
                    for _ in 0..con {
                        self.pop();
                    }
                    let handler = self.pop();
                    for _ in (con+1)..n {
                        self.pop();
                    }
                    for x in data.iter().rev() {
                        self.push(x.clone());
                    }
                    self.push(handler);
                    for _ in 0..data.len() {
                        self.make_app();
                    }
                } else {
                    panic!("matching on non-struct");
                }
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
            self.log.print_eval_unnecessary();
            None
        }
    }

    pub fn print_stack(&self) {
        println!("Vector contents:");
        for (i, n) in self.stack.iter().enumerate() {
            match **n {
                Node::App(ref left, ref right) =>
                    if i + 1 != self.stack.len() && Rc::ptr_eq(&self.stack[i+1], left) {
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
