module Annotate (
    annotate
) where

import Data.Monoid
import AST (Var)
import qualified AST as U
import qualified TypedAST as T
import TypedAST (TypedExprAST((:?)), Type)
import TypedSymbolTable as ST

annotate :: U.FileAST -> T.ScopeAST (Maybe (Type a))
annotate (U.File xs) = T.Scope (buildST xs) (map annotateDecl xs)

annotateDecl :: U.DeclAST -> T.BindingAST (Maybe (Type a))
annotateDecl (U.Decl nm args expr) = T.VarBind nm (map annotateArg args) Nothing (annotateBody expr)
annotateDecl (U.Prim nm) = T.PrimitiveBind nm Nothing
annotateDecl (U.Data nm cons) = T.DataBind nm (map annotateCons cons)

annotateCons :: U.ConsAST -> T.ConsAST (Maybe (Type a))
annotateCons (U.Constructor x n) = T.Constructor x (replicate (fromIntegral n) Nothing)

annotateArg :: Var -> T.ArgAST (Maybe (Type a))
annotateArg x = T.Argument x Nothing

annotateBody :: U.ExprAST -> T.BodyAST (Maybe (Type a))
annotateBody expr = T.ExprBody (annotateExpr expr)

annotateExpr :: U.ExprAST -> T.TypedExprAST (Maybe (Type a))
annotateExpr (U.FunApp l r) = T.FunApp (annotateExpr l) (annotateExpr r) :? Nothing
annotateExpr (U.VarUse x) = T.VarUse x :? Nothing
annotateExpr (U.Num x) = T.Num x :? Nothing
annotateExpr (U.Bool x) = T.Bool x :? Nothing

buildST :: [U.DeclAST] -> ST.SymbolTable (Maybe (Type a))
buildST = mconcat . map buildSTDecl

buildSTDecl :: U.DeclAST -> ST.SymbolTable (Maybe (Type a))
buildSTDecl (U.Decl name _ _) = putGlobal name name Nothing
buildSTDecl (U.Prim name) = putGlobal name name Nothing
buildSTDecl (U.Data name cons) = putGlobal name ("$dtor_" ++ name) Nothing <> mconcat (map buildSTCons cons)
    where buildSTCons (U.Constructor c _) = putGlobal c ("$ctor_" ++ c) Nothing
