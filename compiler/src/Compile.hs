module Compile (
    compile
) where

import AST
import Assembly
import Data.List (elemIndex)

compile :: FileAST -> [ASM]
compile (File xs) = [PushLabelJump "main", Unwind] ++ concatMap compDecl xs

compDecl :: DeclAST -> [ASM]
compDecl (Decl n args body) = Label n : compExpr (reverse args) 0 body ++ [Unwind]

compExpr :: [Var] -> Int -> ExprAST -> [ASM]
compExpr xs n (FunApl lhs rhs) = compExpr xs n lhs ++ compExpr xs (n+1) rhs ++ [MakeApp]
compExpr xs n (VarUse x) = case elemIndex x xs of
                                Just i -> [PushRelative $ fromIntegral $ n+i+1]
                                Nothing -> [PushLabelJump x]
compExpr _ _  (Num i) = [PushConstant i]

