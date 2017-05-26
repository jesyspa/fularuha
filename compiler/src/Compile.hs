module Compile (
    compile
) where

import AST
import Assembly
import Builtins
import SymbolTable as ST

import Control.Monad.Reader

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)

compile :: FileAST -> [ASM]
compile file = runReader (compFile file) (buildSymbolTable file)

compFile :: FileAST -> Reader SymbolTable [ASM]
compFile (File decls) = (\xs -> [PushLabelJump "main", Unwind] ++ concat xs ++ builtinDefs) <$> mapM compDecl decls

compDecl :: DeclAST -> Reader SymbolTable [ASM]
compDecl (Decl n args body) = do
    code <- withReader (register args) $ compExpr 0 body
    return $ Label n : code ++ [Slide (fromIntegral $ length args + 1), Unwind]
compDecl (Prim _) = return []
compDecl (Data tn cons) = do
    xs <- mapM (uncurry $ compCons tn) $ zip [0..] cons
    let argc = fromIntegral $! length cons
        args = replicate argc (PushArg $ argc+1)
        dtor = [Label $ "$dtor_" ++ tn] ++ args ++ [PushArgStrict $ argc+1, ExecBuiltin (Switch argc), Slide $ argc+2, Unwind]
    return $ dtor ++ concat xs

compCons :: String -> Integer -> ConsAST -> Reader SymbolTable [ASM]
compCons _ i (Constructor x s) = return $ [Label label] ++ args ++ [MemAlloc i s, Slide (s+1), Return]
    where label = "$ctor_" ++ x
          args = replicate (fromIntegral s) (PushArg $ s)

compExpr :: Integer -> ExprAST -> Reader SymbolTable [ASM]
compExpr n (FunApl lhs rhs) = (\x y -> x ++ y ++ [MakeApp]) <$> compExpr n rhs <*> compExpr (n+1) lhs
compExpr n (VarUse x) = asks (ST.lookup x) <$$> \response ->
    case response of
        Global -> [PushLabelJump x]
        Local i -> [PushArg $ n+i+1]
        Type -> error $ "unexpected type"
        TypeConstructor _ -> [PushLabelJump $ "$ctor_" ++ x]
        NotFound -> error $ "name not found: " ++ x
compExpr _  (Num i) = return [PushConstant i]
compExpr _  (Bool b) = return [PushBoolConstant b]

