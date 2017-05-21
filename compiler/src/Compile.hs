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
                                Just i -> [PushRelative $ fromIntegral $ n+i+1, GetRight]
                                Nothing -> [PushLabelJump x]
compExpr _ _  (Num i) = [PushConstant i]

strictDefs :: [ASM]
strictDefs = [
        Label "$print",
        PushRelative 1,
        GetRight,
        Eval,
        PushRelative 0,
        ExecBuiltin Print,
        Slide 2,
        Return
    ]
