use asm::Stmt;

pub fn parse(input: &str) -> Vec<Stmt> {
    parser::stmts(input).expect("parse error")
}

peg! parser(r#"
    use asm::Stmt;
    use bytecode::{Inst,Op};
    use std::str::FromStr;

    #[pub]
    stmts -> Vec<Stmt>
        = stmt*

    stmt -> Stmt
        = KEYWORD<"label"> ident:IDENTIFIER WHITESPACE* ":" WHITESPACE* "\n" { Stmt::Label(ident) }
        / KEYWORD<"push goto"> ident:IDENTIFIER WHITESPACE* "\n" { Stmt::PushLabelJump(ident) }
        / ri:raw_inst WHITESPACE* "\n" { Stmt::RawInst(ri) }

    raw_inst -> Inst
        = KEYWORD<"push constant"> n:num_i32 { Inst::PushConstant(n) }
        / KEYWORD<"push bool constant"> b:bool { Inst::PushBoolConstant(b) }
        / KEYWORD<"push relative"> n:num_usize { Inst::PushRelative(n) }
        / KEYWORD<"push jump"> n:num_usize { Inst::PushJump(n, String::from("$$")) }
        / KEYWORD<"push arg"> n:num_usize { Inst::PushArg(n) }
        / KEYWORD<"push arg strict"> n:num_usize { Inst::PushArgStrict(n) }
        / KEYWORD<"mem alloc"> con:num_usize size:num_usize { Inst::MemAlloc(con, size) }
        / KEYWORD<"make app"> { Inst::MakeApp }
        / KEYWORD<"unwind"> { Inst::Unwind }
        / KEYWORD<"slide"> n:num_usize { Inst::Slide(n) }
        / KEYWORD<"get right"> { Inst::GetRight }
        / KEYWORD<"exec builtin"> op:oper { Inst::ExecBuiltin(op) }
        / KEYWORD<"return"> { Inst::Return }
        / KEYWORD<"eval"> { Inst::Eval }
        / KEYWORD<"debug print stack"> { Inst::DebugPrintStack }
        / KEYWORD<"terminate"> { Inst::Terminate }


    num_i32 -> i32
        = n:$("-"?[0-9]+) ![a-zA-Z$] WHITESPACE* { i32::from_str(n).unwrap() } / #expected("i32")
    num_usize -> usize
        = n:$([0-9]+) ![a-zA-Z$] WHITESPACE* { usize::from_str(n).unwrap() } / #expected("usize")
    oper -> Op
        = KEYWORD<"add"> { Op::Add }
        / KEYWORD<"sub"> { Op::Sub }
        / KEYWORD<"mul"> { Op::Mul }
        / KEYWORD<"equal"> { Op::Equal }
        / KEYWORD<"less than"> { Op::LessThan }
        / KEYWORD<"branch"> { Op::Branch }
        / KEYWORD<"switch"> n:num_usize { Op::Switch(n) }
        / KEYWORD<"print"> { Op::Print }
        / #expected("built-in operation")
    bool -> bool
        = KEYWORD<"true"> { true }
        / KEYWORD<"false"> { false }
    KEYWORD<E> = e:E ![a-zA-Z$] WHITESPACE* { e }
    WHITESPACE = #quiet<[ \t]>
    IDENTIFIER -> String
        = s:$(#quiet<[a-zA-Z$][a-zA-Z0-9_]*>) { String::from(s) } / #expected("identifier")
"#);
