use eval_context::*;
use bytecode::Inst;

pub fn evaluate(code: &[Inst]) {
    let mut contexts: Vec<EvalContext> = Vec::new();
    contexts.push(EvalContext::new(code));
    while !contexts.is_empty() {
        let response: Response;
        {
            let context: &mut EvalContext = contexts.last_mut().expect("logic error");
            response = context.run();
        }
        match response {
            Response::Terminate => { println!("Terminate called"); return },
            Response::Return(node) => {
                contexts.pop();
                let context: &mut EvalContext;
                if contexts.is_empty() {
                    println!("Final result: {:?}", node);
                    return;
                } else {
                    context = contexts.last_mut().expect("logic error");
                }
                context.push(node);
            },
            Response::RequestEval(node) => { 
                contexts.push(EvalContext::new_from_tree(code, node))
            },
        }
    }
}
