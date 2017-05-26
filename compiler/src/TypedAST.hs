module TypedAST (
    Var,
    ScopeAST(..),
    BindingAST(..),
    ConsAST(..),
    ArgAST(..),
    BodyAST(..),
    TypedExprAST(..),
    ExprAST(..)
) where

import AST (Var)
import TypedSymbolTable (SymbolTable)

data ScopeAST tp = Scope (SymbolTable tp) [BindingAST tp]
                 deriving (Eq, Ord, Read, Show)

data BindingAST tp = VarBind Var [ArgAST tp] tp (BodyAST tp)
                   | DataBind Var [ConsAST tp]
                   | PrimitiveBind Var tp
                   deriving (Eq, Ord, Read, Show)

data ConsAST tp = Constructor Var [tp]
                deriving (Eq, Ord, Read, Show)

data ArgAST tp = Argument Var tp
               deriving (Eq, Ord, Read, Show)

-- Later on also: ScopeBody ScopeAST ExprAST
data BodyAST tp = ExprBody (TypedExprAST tp)
                deriving (Eq, Ord, Read, Show)

data TypedExprAST tp = ExprAST tp :? tp
                     deriving (Eq, Ord, Read, Show)

data ExprAST tp = FunApp (TypedExprAST tp) (TypedExprAST tp)
                | VarUse String
                | Num Integer
                | Bool Bool
                deriving (Eq, Ord, Read, Show)


