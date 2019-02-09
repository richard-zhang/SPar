{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
module Expr where

data Expr a where
    (:$) :: Expr (a -> b) -> Expr a -> Expr b
    Lam :: (Expr a -> Expr b) -> Expr (a -> b)

    --
    Lit :: Show a => a -> Expr a

    --
    Unit :: Expr ()

    -- Sum
    Inl :: Expr a -> Expr (Either a b)
    Inr :: Expr b -> Expr (Either a b)
    Case :: Expr (a -> c) -> Expr (b -> c) -> Expr (Either a b -> c)

    -- Product
    Fst :: Expr (a, b) -> Expr a
    Snd :: Expr (a, b) -> Expr b
    Pair :: Expr a -> Expr b -> Expr (a, b)
    Split :: Expr (a -> b) -> Expr (a -> c) -> Expr (a -> (b, c))

    Prim :: String -> (a -> b) -> Expr (a -> b)

    -- use internally
    Val :: a -> Expr a
    Var :: Integer -> Expr a

instance (Num a, Show a) => Num (Expr a) where
    a + b = Prim "(+)" (+) :$ a :$ b
    a - b = Prim "(-)" (-) :$ a :$ b
    a * b = Prim "(*)" (*) :$ a :$ b
    abs a = Prim "abs" abs :$ a
    signum a = Prim "signum" signum :$ a
    fromInteger = Lit . fromInteger

len :: Expr [a] -> Expr Int
len list = Prim "length" length :$ list

true :: Expr (Either () ())
true = Inl Unit

false :: Expr (Either () ())
false = Inr Unit

smaller :: Ord a => Expr a -> Expr a -> Expr (Either () ())
smaller a b = Prim "<" small :$ a :$ a
    where small a b = if a > b then Left () else Right ()
