use std::io;
mod eval_context;

fn main() {
    let mut ec = eval_context::EvalContext::new();

    loop {
        let mut cmd = String::new();
        io::stdin().read_line(&mut cmd)
            .expect("read error");

        let cmd = cmd.trim();

        if cmd == "MkApp" {
            ec.make_app();
        } else if cmd.starts_with("PushConstant") {
            let cstr = cmd.split(' ').nth(1).expect("constant expected");
            ec.push_constant(cstr.parse().expect("constant not numeric"));
        } else if cmd == "Print" {
            ec.print_stack();
        } else {
            println!("Unknown command: {}", cmd);
        }
    }
}
