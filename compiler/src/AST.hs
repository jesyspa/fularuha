module AST (
    Var,
    FileAST(..),
    DeclAST(..),
    ConsAST(..),
    ExprAST(..)
) where

type Var = String

data FileAST = File [DeclAST]
             deriving (Eq, Ord, Read, Show)

data DeclAST = Decl Var [Var] ExprAST
             | Prim Var
             | Data Var [ConsAST]
             deriving (Eq, Ord, Read, Show)

data ConsAST = Constructor Var Integer
             deriving (Eq, Ord, Read, Show)

data ExprAST = FunApp ExprAST ExprAST
             | VarUse Var
             | Num Integer
             | Bool Bool
             deriving (Eq, Ord, Read, Show)


