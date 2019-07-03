{-# LANGUAGE GADTs #-}
module Language.Poly.Core2 where
import           CodeGen.Type

data Core a where
    Lit :: a -> Core a
    Var :: Int -> Core a

    Prim  :: String -- XXX: name of the "primitive function"
             -> a -- Semantics in Haskell of the primitive function
             -> Core a

    (:$) :: Core (a -> b) -> Core a -> Core b
    Comp :: Core (b -> c) -> Core (a -> b) -> Core (a -> c)

    Id :: Core (a -> a)
    Const :: Core a -> Core (b -> a)

    Fst :: Core ((a, b) -> a)
    Snd :: Core ((a, b) -> b)
    Pair :: Core a -> Core b -> Core (a, b)

    Inl :: Core (a -> Either a b)
    Inr :: Core (b -> Either a b)

    Swap :: Core (((a,b), (c, d)) -> ((a, c), (b, d)))
    (:&&&) :: Core (a -> b) -> Core (a -> c) -> Core (a -> (b, c))
    (:***) :: Core (a -> c) -> Core (b -> d) -> Core ((a, b) -> (c, d))
