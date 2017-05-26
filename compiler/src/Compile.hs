module Compile (
    compile
) where

import TypedAST
import Assembly
import Builtins
import TypedSymbolTable

import Data.Monoid
import Control.Monad.Reader

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)

compile :: ScopeAST a -> [ASM]
compile file = runReader (compScope file) mempty

compScope :: ScopeAST a -> Reader (SymbolTable a) [ASM]
compScope (Scope st binds) = do
    xs <- withReader (<> st) $ mapM compBind binds
    return $ [PushLabelJump "main", Unwind] ++ concat xs ++ builtinDefs

compBind :: BindingAST a -> Reader (SymbolTable a) [ASM]
compBind (VarBind name args _ body) = do
    code <- withReader (<> putArgs (map (\(Argument x tp) -> (x, tp)) args)) $ compBody body
    return $ Label name : code ++ [Slide (fromIntegral $ length args + 1), Unwind]
compBind (PrimitiveBind _ _) = return []
compBind (DataBind name cons) = do
    xs <- mapM (uncurry $ compCons name) $ zip [0..] cons
    let argc = fromIntegral $ length cons
        args = replicate argc (PushArg $ argc+1)
        dtor = [Label $ "$dtor_" ++ name] ++ args ++ [PushArgStrict $ argc+1, ExecBuiltin (Switch argc), Slide $ argc+2, Unwind]
    return $ dtor ++ concat xs

compCons :: String -> Integer -> ConsAST a -> Reader (SymbolTable a) [ASM]
compCons _ i (Constructor x args) = return $ [Label label] ++ argsCode ++ [MemAlloc i argc, Slide (argc+1), Return]
    where label = "$ctor_" ++ x
          argsCode = map (const $ PushArg argc) args
          argc = fromIntegral $ length args

compBody :: BodyAST a -> Reader (SymbolTable a) [ASM]
compBody (ExprBody expr) = compExpr 0 expr

compExpr :: Integer -> TypedExprAST a -> Reader (SymbolTable a) [ASM]
compExpr n (FunApp lhs rhs :? _) = (\x y -> x ++ y ++ [MakeApp]) <$> compExpr n rhs <*> compExpr (n+1) lhs
compExpr n (VarUse x :? _) = asks (lookupValue x) <$$> \response ->
    case response of
        Global label _ -> [PushLabelJump label]
        Local i _ -> [PushArg $ n+i+1]
        NotFound -> error $ "name not found: " ++ x
compExpr _  (Num i :? _) = return [PushConstant i]
compExpr _  (Bool b :? _) = return [PushBoolConstant b]

