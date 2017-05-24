use std::rc::Rc;
use bytecode::Inst;
use eval_context::*;
use debug_log::DebugLog;

pub fn evaluate<'a>(code: &'a [Inst], log: &'a DebugLog) -> Option<Rc<Node<'a>>> {
    let mut contexts: Vec<EvalContext> = Vec::new();
    contexts.push(EvalContext::new(log));
    let mut i = 0;
    while !contexts.is_empty() {
        let response: Response;
        {
            let context: &mut EvalContext = contexts.last_mut().expect("logic error");
            response = context.run(code);
        }
        log.print_response(&response);
        match response {
            Response::Terminate => return None,
            Response::Return(node) => {
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
                contexts.push(EvalContext::new_from_tree(log, node))
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

