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
        deriving (Eq, Ord, Read, Show)

ppOp :: Op -> Doc
ppOp Print    = text "print"
ppOp Add      = text "add"
ppOp Mul      = text "mul"
ppOp Sub      = text "sub"
ppOp Equal    = text "equal"
ppOp LessThan = text "less than"
ppOp Branch   = text "branch"


data ASM = Label String
         | PushLabelJump String
         | PushConstant Integer
         | PushRelative Integer
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
ppASM (PushRelative i) = text "push relative" <+> integer i
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
