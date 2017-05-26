module Annotate (
    annotate
) where

import Data.Monoid
import AST (Var)
import qualified AST as U
import qualified TypedAST as T
import TypedAST (TypedExprAST((:?)))
import TypedSymbolTable as ST

annotate :: U.FileAST -> T.ScopeAST ()
annotate (U.File xs) = T.Scope (buildST xs) (map annotateDecl xs)

annotateDecl :: U.DeclAST -> T.BindingAST ()
annotateDecl (U.Decl nm args expr) = T.VarBind nm (map annotateArg args) () (annotateBody expr)
annotateDecl (U.Prim nm) = T.PrimitiveBind nm ()
annotateDecl (U.Data nm cons) = T.DataBind nm (map annotateCons cons)

annotateCons :: U.ConsAST -> T.ConsAST ()
annotateCons (U.Constructor x n) = T.Constructor x (replicate (fromIntegral n) ())

annotateArg :: Var -> T.ArgAST ()
annotateArg x = T.Argument x ()

annotateBody :: U.ExprAST -> T.BodyAST ()
annotateBody expr = T.ExprBody (annotateExpr expr)

annotateExpr :: U.ExprAST -> T.TypedExprAST ()
annotateExpr (U.FunApp l r) = T.FunApp (annotateExpr l) (annotateExpr r) :? ()
annotateExpr (U.VarUse x) = T.VarUse x :? ()
annotateExpr (U.Num x) = T.Num x :? ()
annotateExpr (U.Bool x) = T.Bool x :? ()

buildST :: [U.DeclAST] -> ST.SymbolTable ()
buildST = mconcat . map buildSTDecl

buildSTDecl :: U.DeclAST -> ST.SymbolTable ()
buildSTDecl (U.Decl name _ _) = putGlobal name name ()
buildSTDecl (U.Prim name) = putGlobal name name ()
buildSTDecl (U.Data name cons) = putGlobal name ("$dtor_" ++ name) () <> mconcat (map buildSTCons cons)
    where buildSTCons (U.Constructor c _) = putGlobal c ("$ctor_" ++ c) ()
