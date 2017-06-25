module TypedAST (
    Var,
    Type(..),
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

data Type a = TArrow (Type a) (Type a)
            | TLiteral a
            deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data ScopeAST tp = Scope (SymbolTable tp) [BindingAST tp]
                 deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data BindingAST tp = VarBind Var [ArgAST tp] tp (BodyAST tp)
                   | DataBind Var [ConsAST tp]
                   | PrimitiveBind Var tp
                   deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data ConsAST tp = Constructor Var [tp]
                deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data ArgAST tp = Argument Var tp
               deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- Later on also: ScopeBody ScopeAST ExprAST
data BodyAST tp = ExprBody (TypedExprAST tp)
                deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data TypedExprAST tp = ExprAST tp :? tp
                     deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data ExprAST tp = FunApp (TypedExprAST tp) (TypedExprAST tp)
                | VarUse String
                | Num Integer
                | Bool Bool
                deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)


