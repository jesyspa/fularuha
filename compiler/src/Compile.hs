module Compile (
    compile
) where

import AST
import Assembly
import Data.List (elemIndex)

compile :: FileAST -> [ASM]
compile (File xs) = [PushLabelJump "main", Unwind] ++ concatMap compDecl xs ++ strictDefs

compDecl :: DeclAST -> [ASM]
compDecl (Decl n args body) = Label n : compExpr args 0 body ++ [Slide (fromIntegral $ length args + 1), Unwind]

compExpr :: [Var] -> Int -> ExprAST -> [ASM]
compExpr xs n (FunApl lhs rhs) = compExpr xs n rhs ++ compExpr xs (n+1) lhs ++ [MakeApp]
compExpr xs n (VarUse x) = case elemIndex x xs of
                                Just i -> [PushArg $ fromIntegral $ n+i+1]
                                Nothing -> [PushLabelJump x]
compExpr _ _  (Num i) = [PushConstant i]
compExpr _ _  (Bool b) = [PushBoolConstant b]

binaryOp :: String -> Op -> [ASM]
binaryOp name op =
    [ Label name
    , PushArgStrict 2
    , PushArgStrict 2
    , ExecBuiltin op
    , Slide 3
    , Return
    ]

strictDefs :: [ASM]
strictDefs =
    [ Label "$print"
    , PushArgStrict 1
    , PushRelative 0
    , ExecBuiltin Print
    , Slide 2
    , Return
    , Label "$branch"
    , PushArg 3
    , PushArg 3
    , PushArgStrict 3
    , ExecBuiltin Branch
    , Slide 4
    , Unwind
    , Label "$seq"
    , PushArgStrict 2
    , PushArg 2
    , MakeApp
    , Slide 3
    , Unwind
    , Label "$nil"
    , MemAlloc 0 0
    , Slide 1
    , Return
    , Label "$cons"
    , PushArg 2
    , PushArg 2
    , MemAlloc 1 2
    , Slide 3
    , Return
    , Label "$match_list"
    , PushArg 3
    , PushArg 3
    , PushArgStrict 3
    , ExecBuiltin (Switch 2)
    , Slide 4
    , Unwind
    ] ++ concatMap (uncurry binaryOp) [("$mul", Mul), ("$add", Add), ("$sub", Sub), ("$less_than", LessThan), ("$equal", Equal)]
