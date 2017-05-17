use std::io;

#[derive(Debug)]
enum Node {
    App(Box<Node>, Box<Node>),
    Num(i32),
}

fn make_app(v: &mut Vec<Box<Node>>) {
    let right = v.pop().expect("stack underflow");
    let left = v.pop().expect("stack underflow");
    v.push(Box::new(Node::App(left, right)));
}

fn push_constant(v: &mut Vec<Box<Node>>, c: i32) {
    v.push(Box::new(Node::Num(c)));
}

fn print_stack(v: &Vec<Box<Node>>) {
    println!("Vector contents:");
    for n in v {
        println!("{:?}", n);
    }
}

fn main() {
    let mut v: Vec<Box<Node>> = Vec::new();

    loop {
        let mut cmd = String::new();
        io::stdin().read_line(&mut cmd)
            .expect("read error");

        let cmd = cmd.trim();

        if cmd == "MkApp" {
            make_app(&mut v);
        } else if cmd.starts_with("PushConstant") {
            let cstr = cmd.split(' ').nth(1).expect("constant expected");
            push_constant(&mut v, cstr.parse().expect("constant not numeric"));
        } else if cmd == "Print" {
            print_stack(&v);
        } else {
            println!("Unknown command: {}", cmd);
        }
    }
}
