{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Def where

import           Data.Type.Equality
import           Data.Proxy
import           Prelude                 hiding ( (>>)
                                                , (>>=)
                                                , return
                                                )
import           Data.Singletons.TypeLits
import           Data.Kind
import qualified Data.Map.Strict               as Map
import           GHC.Natural
import           Language.Poly.Core             ( Core(..) )
import qualified GHC.TypeLits
import           Type
import           Control.Monad.Free
import           Control.Monad.Indexed
import qualified Control.Monad.Indexed.Free    as F

type CC a = (Show a, Read a)

data ProcF (i :: SType Type) (j :: SType Type) next where
    Send :: (CC a) => Sing (n :: Nat) -> Core a -> next -> ProcF ('Free ('S n a j)) j next
    Recv :: (CC a) => Sing (n :: Nat) -> (Core a -> next) -> ProcF ('Free ('R n a j)) j next
    Branch :: (CC c) => Sing (n :: Nat) -> Proc' left ('Pure ()) c -> Proc' right ('Pure ()) c -> next -> ProcF ('Free ('B n left right j)) j next
    Select :: (CC a, CC b, CC c) => Sing (n :: Nat) -> Core (Either a b) -> (Core a -> Proc' left ('Pure ()) c) -> (Core b -> Proc' right ('Pure ()) c) -> next -> ProcF ('Free ('Se n left right j)) j next

type Proc' i j a = F.IxFree ProcF i j (Core a)
type Proc i a = forall j. F.IxFree ProcF (i >*> j) j (Core a)

instance Functor (ProcF i j) where
    fmap f (Send a v n) = Send a v $ f n
    fmap f (Recv a cont) = Recv a (f . cont)
    fmap f (Branch r left right n) = Branch r left right $ f n
    fmap f (Select r v cont1 cont2 next) = Select r v cont1 cont2 (f next)

instance IxFunctor ProcF where
    imap = fmap

liftF' :: IxFunctor f => f i j a -> F.IxFree f i j a
liftF' = F.Free . imap F.Pure

send :: (CC a) => Sing n -> Core a -> Proc ( 'Free ( 'S n a ( 'Pure ()))) a
send role value = liftF' $ Send role value value

recv :: (CC a) => Sing n -> Proc ( 'Free ( 'R n a ( 'Pure ()))) a
recv role = liftF' (Recv role id)

select
    :: (CC a, CC b, CC c)
    => Sing n
    -> Core (Either a b)
    -> (Core a -> Proc' left ( 'Pure ()) c)
    -> (Core b -> Proc' right ( 'Pure ()) c)
    -> Proc ( 'Free ( 'Se n left right ( 'Pure ()))) ()
select role var cont1 cont2 = liftF' $ Select role var cont1 cont2 Unit

branch
    :: (Show c, Read c)
    => Sing n
    -> Proc' left ( 'Pure ()) c
    -> Proc' right ( 'Pure ()) c
    -> Proc ( 'Free ( 'B n left right ( 'Pure ()))) ()
branch role one two = liftF' $ Branch role one two Unit

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

(>>) :: IxMonad m => m i j b -> m j k1 b1 -> m i k1 b1
a >> b = a >>= const b

return :: IxMonad m => a -> m i i a
return = ireturn

test :: Proc ('Free ('S 1 Integer ('Free ('R 1 (Either () ()) ('Free ('Se 2 ('Free ('R 2 Integer ('Pure ()))) ('Free ('S 2 Integer ('Pure ()))) ('Free ('S 1 Integer ('Pure ()))))))))) ()
test = do
    send (SNat :: Sing 1) (Lit 10)
    x :: Core (Either () ()) <- recv (SNat :: Sing 1)
    select (SNat :: Sing 2)
           x
           (\_ -> recv (SNat :: Sing 2))
           (\_ -> send (SNat :: Sing 2) (Lit 30))
    send (SNat :: Sing 1) (Lit 10)
    return Unit

test1 = do
    x :: Core Integer <- recv (SNat :: Sing 0)
    send (SNat :: Sing 0) (Lit (Left () :: Either () ()))
    return Unit

test2 :: Proc ('Free ('B 0 ('Free ('S 2 Integer ('Pure ()))) ('Free ('R 2 Integer ('Pure ()))) ('Pure ()))) ()
test2 = branch (SNat :: Sing 0) a b

test3 = do
    send (SNat :: Sing 1) (Lit 10)
    send (SNat :: Sing 1) (Lit "Str")
    send (SNat :: Sing 1) (Lit 'c')

a :: Proc ( 'Free ( 'S 2 Integer ('Pure ()))) Integer
a = send (SNat :: Sing 2) (Lit 20)

b :: Proc ( 'Free ( 'R 2 Integer ('Pure ()))) Integer
b = recv (SNat :: Sing 2) 