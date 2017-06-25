module Inference (
    inferTypes
) where

import Data.Either (either)
import TypedAST
import Control.Monad.State

data InferredType a = Unknown Int
                    | IArrow (InferredType a) (InferredType a)
                    | ILiteral a

inject :: Type a -> InferredType a
inject (TArrow x y) = IArrow (inject x) (inject y)
inject (TLiteral a) = ILiteral a

freelyAnnotate :: Traversable t => t (Maybe (Type a)) -> t (InferredType a)
freelyAnnotate tree = evalState (traverse m tree) 0
    where m Nothing = get >>= \i -> put (i+1) >> return (Unknown i)
          m (Just x) = return $ inject x

unify :: ScopeAST (InferredType a) -> ScopeAST (InferredType a)
unify = id

extract :: Functor f => f (InferredType a) -> f (Type (Either Int a))
extract = fmap f
    where f (Unknown i) = TLiteral (Left i)
          f (IArrow x y) = TArrow (f x) (f y)
          f (ILiteral a) = TLiteral (Right a)

-- Maybe generalise this later
inferTypes :: ScopeAST (Maybe (Type String)) -> ScopeAST (Type String)
inferTypes = fmap (fmap $ either (("$inferred_"++) . show) id) . extract . unify . freelyAnnotate
