{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Language.Poly.Nat (module Language.Poly.Nat, module Data.Type.Natural) where
import qualified GHC.TypeLits as GT
import           Type.Reflection
import           CodeGen.Type
import           Data.Proxy
import           Data.Type.Natural
import           Data.Type.Natural.Class.Order
import           Proof.Propositional
import           Data.Singletons.Prelude.Enum
import           Data.Type.Equality

newtype Unit a = MkUnit () deriving Show

unit :: Unit a
unit = MkUnit ()

type family Prod (n :: Nat) (a :: *) = r | r -> n a where
    Prod 'Z a = Unit a
    Prod ('S n) a = (a, Prod n a)

type family Div2 (n :: Nat) where
    Div2 'Z = 'Z
    Div2 ('S 'Z) = 'Z
    Div2 ('S ('S n)) = 'S (Div2 n)

type family Add (n :: Nat) (m :: Nat) where
    Add 'Z n = n
    Add ('S n) m = 'S (Add n m)

toNat :: (SNat n) -> Int
toNat SZ     = 0
toNat (SS n) = 1 + toNat n

addn :: SNat n -> SNat m -> SNat (Add n m)
addn SZ     n = n
addn (SS n) m = SS (addn n m)

type family Mul (n :: Nat) (m :: Nat) where
    Mul 'Z n = 'Z
    Mul ('S n) m = Add m (Mul n m)

mul :: SNat n -> SNat m -> SNat (Mul n m)
mul SZ     n = SZ
mul (SS n) m = addn m (mul n m)

type family Pow2 (n :: Nat) where
    Pow2 'Z = 'S 'Z
    Pow2 ('S n) = Add (Pow2 n) (Pow2 n)

pow2 :: SNat n -> SNat (Pow2 n)
pow2 SZ     = SS SZ
pow2 (SS n) = addn (pow2 n) (pow2 n)

type family ToNat (i :: Nat) :: GT.Nat where
    ToNat 'Z = 0
    ToNat ('S n) = 1 + ToNat n

type family FromNat (i :: GT.Nat) :: Nat where
    FromNat 0 = 'Z
    FromNat n = 'S (FromNat (n-1))

type family Tree (n :: Nat) (a :: *) where
  Tree 'Z a = a
  Tree ('S n) a = (Tree n a, Tree n a)

szr :: SNat n -> Add n Z :~: n
szr SZ     = Refl
szr (SS n) = case szr n of
    Refl -> Refl

ssr :: SNat n -> SNat m -> Add n (S m) :~: S (Add n m)
ssr SZ     _ = Refl
ssr (SS n) m = case ssr n m of
    Refl -> Refl

addComm :: SNat n -> SNat m -> Add n m :~: Add m n
addComm SZ m = case szr m of
    Refl -> Refl -- unsafeCoerce Refl
addComm (SS n) m = case (addComm n m, ssr m n) of
    (Refl, Refl) -> Refl

fromNat :: forall a n . Num a => SNat n -> a
fromNat SZ     = 0
fromNat (SS x) = 1 + fromNat x

data SDict a where
    SDict :: Serialise a => SDict a

getSDict :: Serialise a => SNat n -> Proxy a -> SDict (Tree n a)
getSDict SZ     _ = SDict
getSDict (SS n) p = case getSDict n p of
    SDict -> SDict


-- proof

gtZero :: Compare m n :~: 'LT -> SNat m -> SNat n -> IsTrue (Zero < (n - m))
gtZero comp SZ a = case aminuszero of
    Refl -> lt
  where
    lt         = ltToLneq SZ a comp
    aminuszero = minusZero a
gtZero comp (SS m) (SS n) = case cmpSucc m n of
    Refl -> gtZero comp m n
gtZero _ _ _ = error "bad gtzero"

pred2
    :: Compare m n :~: 'LT
    -> SNat m
    -> SNat n
    -> (Pred (n - m)) :~: (n - ( 'S m))
pred2 _    SZ     (SS _) = Refl
pred2 comp (SS m) (SS n) = case cmpSucc m n of
    Refl -> pred2 comp m n
pred2 _ _ _ = error "bad pred2"

sMinus :: Compare m n :~: 'LT -> SNat n -> SNat m -> SNat (n - m)
sMinus _comp n      SZ     = n
sMinus comp  (SS n) (SS m) = sMinus b n m
  where
    a = cmpSucc m n
    b = a `trans` comp
sMinus _comp SZ _ = error "not gonna happen"

type family Arity t :: Nat where
    Arity (i -> o) = 'S (Arity o) 
    Arity t = 'Z

arity :: SingI (Arity a) => Proxy a -> SNat (Arity a)
arity _ = sing

intArity :: SingI (Arity a) => Proxy a -> Int
intArity = sNatToInt . arity