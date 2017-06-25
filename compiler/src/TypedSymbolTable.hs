module TypedSymbolTable (
    SymbolTable,
    LookupValueResult(..),
    putArgs,
    putGlobal,
    lookupValue
) where

import AST (Var)

import Data.Monoid
import Data.Maybe
import Control.Applicative
import qualified Data.Map as M

data Entry tp = Entry Var tp
           deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data SymbolTable tp = SymbolTable (M.Map Var (Entry tp)) [Entry tp]
                    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Monoid (SymbolTable tp) where
    mempty = SymbolTable M.empty []
    SymbolTable g1 a1 `mappend` SymbolTable g2 a2 = SymbolTable (g1 <> g2) (a1 <> a2)

data LookupValueResult tp
    = Global Var tp
    | Local Integer tp
    | NotFound
    deriving (Eq, Ord, Read, Show)

-- TODO: Use lenses here.

putArgs :: [(Var, tp)] -> SymbolTable tp
putArgs = SymbolTable M.empty . map f
    where f (v, t) = Entry v t

putGlobal :: Var -> Var -> tp -> SymbolTable tp
putGlobal v name tp = SymbolTable (M.singleton v (Entry name tp)) []

lookupValue :: Var -> SymbolTable tp -> LookupValueResult tp
lookupValue v (SymbolTable gs as) = fromMaybe NotFound $ global <|> arg
    where global = (\(Entry name tp) -> Global name tp) <$> M.lookup v gs
          arg = uncurry Local <$> lookupEntry v as


lookupEntry :: Var -> [Entry tp] -> Maybe (Integer, tp)
lookupEntry = lookupEntry' 0

lookupEntry' :: Integer -> Var -> [Entry tp] -> Maybe (Integer, tp)
lookupEntry' _ _ [] = Nothing
lookupEntry' i v (Entry w tp : xs) | v == w = Just (i, tp)
                                   | otherwise = lookupEntry' (i+1) v xs
