module AST (
    Var,
    FileAST(..),
    DeclAST(..),
    ExprAST(..)
) where

type Var = String

data FileAST = File [DeclAST]
             deriving (Eq, Ord, Read, Show)

data DeclAST = Decl Var [Var] ExprAST
             deriving (Eq, Ord, Read, Show)

data ExprAST = FunApl ExprAST ExprAST
             | VarUse Var
             | Num Integer
             deriving (Eq, Ord, Read, Show)


