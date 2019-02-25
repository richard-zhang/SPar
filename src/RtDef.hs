{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}
module RtDef where

import Control.Monad.Free
import qualified Control.Monad.Indexed.Free as F
import Data.Singletons
import Data.Type.Natural (Nat)
import Data.Typeable
import Def
import Language.Poly.Core
import Type
import TypeValue

type ProcessRT a = (ProcRT a, Nat)

data ProcRTF next where
    Send' :: (Serialise a) => Nat -> Core a -> next -> ProcRTF next
    Recv' :: (Serialise a) => Nat -> (Core a -> next) -> ProcRTF next
    Select' :: (Serialise a, Serialise b, Serialise c) => Nat -> Core (Either a b) -> (Core a -> ProcRT c) -> (Core b -> ProcRT c) -> next -> ProcRTF next
    Branch' :: (Serialise c) => Nat -> ProcRT c -> ProcRT c -> next -> ProcRTF next

instance Functor ProcRTF where
    fmap f (Send' r v n)               = Send' r v $ f n
    fmap f (Recv' r cont)              = Recv' r (f . cont)
    fmap f (Select' r v cont1 cont2 n) = Select' r v cont1 cont2 (f n)
    fmap f (Branch' r left right n)         = Branch' r left right (f n)

type ProcRT a = Free ProcRTF (Core a)

send' :: Serialise a => Nat -> Core a -> ProcRT a
send' role value = liftF $ Send' role value value

recv' :: Serialise a => Nat -> ProcRT a
recv' role = liftF $ Recv' role id

select' :: (Serialise a, Serialise b, Serialise c) => Nat -> Core (Either a b) -> (Core a -> ProcRT c) -> (Core b -> ProcRT c) -> ProcRT ()
select' role var cont1 cont2 = liftF $ Select' role var cont1 cont2 Unit

branch' :: Serialise c => Nat -> ProcRT c -> ProcRT c -> ProcRT ()
branch' role left right = liftF $ Branch' role left right Unit

convert :: Integer -> ProcRT a -> STypeV ()
convert _ (Pure a) = Pure ()
convert n (Free (Send' r v next)) =
    Free (S r (typeRep $ extractType v) $ convert n next)
convert n (Free (Recv' r cont)) = Free
    (R r (typeRep $ extractParamType cont) $ convert (n + 1) (cont $ Var n))
convert n (Free (Select' r v cont1 cont2 next)) = Free
    (Se r
        (convert 1 (cont1 $ Var 0))
        (convert 1 (cont2 $ Var 0))
        (convert n next)
    )
convert n (Free (Branch' r left right next)) =
    Free (B r (convert 0 left) (convert 0 right) (convert n next))

eraseSessionInfo' :: Proc' i j a -> ProcRT a
eraseSessionInfo' (F.Pure v) = Pure v
eraseSessionInfo' (F.Free (Send (r :: Sing (n :: Nat)) v next)) =
    Free (Send' (fromSing r) v (eraseSessionInfo' next))
eraseSessionInfo' (F.Free (Recv (r :: Sing (n :: Nat)) cont)) =
    Free (Recv' (fromSing r) (eraseSessionInfo' . cont))
eraseSessionInfo' (F.Free (Select (r :: Sing (n :: Nat)) v cont1 cont2 next)) =
    Free
        (Select' (fromSing r)
                    v
                    (eraseSessionInfo' . cont1)
                    (eraseSessionInfo' . cont2)
                    (eraseSessionInfo' next)
        )
eraseSessionInfo' (F.Free (Branch (r :: Sing (n :: Nat)) left right next)) =
    Free
        (Branch' (fromSing r)
                    (eraseSessionInfo' left)
                    (eraseSessionInfo' right)
                    (eraseSessionInfo' next)
        )

eraseSessionInfo :: Process k a -> ProcessRT a
eraseSessionInfo (Process n value) = (eraseSessionInfo' value, fromSing n)
