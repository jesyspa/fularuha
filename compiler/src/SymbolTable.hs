{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}
module SymbolTable (
    SymbolTable,
    LookupResult(..),
    buildSymbolTable,
    register,
    SymbolTable.lookup
) where

import AST

import Data.Maybe
import Data.Monoid
import Control.Applicative
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (elemIndex)

data SymbolTable = SymbolTable
    { localST :: [String]
    , globalST :: GlobalSymbolTable
    }

data GlobalSymbolTable = GlobalSymbolTable
    { globals :: S.Set String
    , types :: S.Set String
    , typeConstructors :: M.Map String Integer
    }
    deriving({-! Monoid !-})

data LookupResult
    = Global
    | Local Integer
    | Type
    | TypeConstructor Integer
    | NotFound
    deriving (Eq, Ord, Read, Show)

mkGlobal :: String -> GlobalSymbolTable
mkGlobal s = mempty { globals = S.singleton s }

mkType :: String -> GlobalSymbolTable
mkType s = mempty { types = S.singleton s }

mkTypeConstructor :: String -> Integer -> GlobalSymbolTable
mkTypeConstructor s i = mempty { typeConstructors = M.singleton s i }

buildSymbolTable :: FileAST -> SymbolTable
buildSymbolTable (File xs) = SymbolTable [] (mconcat $ map buildDeclST xs)

buildDeclST :: DeclAST -> GlobalSymbolTable
buildDeclST (Decl fun _ _) = mkGlobal fun
buildDeclST (Prim prim) = mkGlobal prim
buildDeclST (Data tn cons) = mkType tn <> mkGlobal ("$dtor_" ++ tn) <> mconcat (map buildConsST cons)

buildConsST :: ConsAST -> GlobalSymbolTable
buildConsST (Constructor name arity) = mkTypeConstructor name arity

register :: [String] -> SymbolTable -> SymbolTable
register xs st = st { localST = xs }

lookup :: String -> SymbolTable -> LookupResult
lookup s (SymbolTable lst gst) = fromMaybe NotFound $
    (Local . fromIntegral) <$> elemIndex s lst
    <|> Global <$ wrap (S.member s $ globals gst)
    <|> Type <$ wrap (S.member s $ types gst)
    <|> TypeConstructor <$> M.lookup s (typeConstructors gst)
    where wrap True = Just ()
          wrap False = Nothing
