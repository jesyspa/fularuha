use std::rc::Rc;
use bytecode::Inst;
use eval_context::*;

pub fn evaluate(code: &[Inst]) -> Option<Rc<Node>> {
    let mut contexts: Vec<EvalContext> = Vec::new();
    contexts.push(EvalContext::new(code));
    let mut i = 0;
    while !contexts.is_empty() {
        let response: Response;
        {
            let context: &mut EvalContext = contexts.last_mut().expect("logic error");
            response = context.run();
        }
        match response {
            Response::Terminate => { println!("Terminate called"); return None },
            Response::Return(node) => {
                println!("Returning: {:?}", node);
                contexts.pop();
                let context: &mut EvalContext;
                if contexts.is_empty() {
                    return Some(node);
                } else {
                    context = contexts.last_mut().expect("logic error");
                }
                context.push(node);
            },
            Response::RequestEval(node) => { 
                println!("Evaluating: {:?}", node);
                contexts.push(EvalContext::new_from_tree(code, node))
            },
        }
        i += 1;
        // This is really handy for debugging.
        if i > 1000 {
            panic!("Too many steps");
        }
    }
    None
}

