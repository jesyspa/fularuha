module Assembly (
    Op(..),
    ASM(..),
    ppASMs
) where

import Text.PrettyPrint

data Op = Print
        | Add
        | Mul
        | Sub
        | Equal
        | LessThan
        | Branch
        | Switch Integer
        deriving (Eq, Ord, Read, Show)

ppOp :: Op -> Doc
ppOp Print      = text "print"
ppOp Add        = text "add"
ppOp Mul        = text "mul"
ppOp Sub        = text "sub"
ppOp Equal      = text "equal"
ppOp LessThan   = text "less than"
ppOp Branch     = text "branch"
ppOp (Switch n) = text "switch" <+> integer n


data ASM = Label String
         | PushLabelJump String
         | PushConstant Integer
         | PushBoolConstant Bool
         | PushRelative Integer
         | PushArg Integer
         | PushArgStrict Integer
         | MemAlloc Integer Integer
         | MakeApp
         | Unwind
         | Slide Integer
         | ExecBuiltin Op
         | Eval
         | GetRight
         | Return
         | Terminate
         deriving (Eq, Ord, Read, Show)

ppASM :: ASM -> Doc
ppASM (Label xs) = text "label" <+> text xs <> char ':'
ppASM (PushLabelJump xs) = text "push goto" <+> text xs
ppASM (PushConstant i) = text "push constant" <+> integer i
ppASM (PushBoolConstant b) = text "push bool constant" <+> text (if b then "true" else "false")
ppASM (PushRelative i) = text "push relative" <+> integer i
ppASM (PushArg i) = text "push arg" <+> integer i
ppASM (PushArgStrict i) = text "push arg strict" <+> integer i
ppASM (MemAlloc con size) = text "mem alloc" <+> integer con <+> integer size
ppASM MakeApp = text "make app"
ppASM Unwind = text "unwind"
ppASM (Slide i) = text "slide" <+> integer i
ppASM (ExecBuiltin op) = text "exec builtin" <+> ppOp op
ppASM Eval = text "eval"
ppASM GetRight = text "get right"
ppASM Return = text "return"
ppASM Terminate = text "terminate"

ppASMs :: [ASM] -> Doc
ppASMs xs = vcat (map ppASM xs) <> char '\n'
