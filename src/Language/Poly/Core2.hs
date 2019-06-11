{-# LANGUAGE GADTs #-}
module Language.Poly.Core2 where
import           Data.Type.Natural
import           Control.Monad.Free

data Core a where
    Lit :: a -> Core a
    Var :: Int -> Core a

    Prim  :: String -- XXX: name of the "primitive function"
             -> a -- Semantics in Haskell of the primitive function
             -> Core a

    Ap :: Core (a -> b) -> Core a -> Core b

    Id :: Core (a -> a)
    Const :: Core a -> Core (b -> a)

    Fst :: Core ((a, b) -> a)
    Snd :: Core ((a, b) -> b)
    Pair :: Core a -> Core b -> Core (a, b)

    Inl :: Core (a -> Either a b)
    Inr :: Core (b -> Either a b)



data ProcF next where
    Send :: Nat -> Core a -> next -> ProcF next

    Recv :: Nat -> (Core a -> next) -> ProcF next

    Select :: Nat
           -> Core (Either a b)
           -> (Core a -> Proc c)
           -> (Core b -> Proc c)
           -> next
           -> ProcF next

    Branch :: Nat -> Proc c -> Proc c -> (Core c -> next) -> ProcF next

    Broadcast :: [Nat]
              -> Core (Either a b)
              -> (Core a -> Proc c)
              -> (Core b -> Proc c)
              -> (Core c -> next)
              -> ProcF next

type Proc a = Free ProcF (Core a)
