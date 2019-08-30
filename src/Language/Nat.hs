{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Language.Nat where
import           GHC.TypeLits
import           Type.Reflection
import           CodeGen.Type
import           Data.Proxy

data INat = Z | S INat

data family Sing :: k -> *

data instance Sing (n :: INat) where
    SZ :: Sing 'Z
    SS :: Sing n -> Sing ('S n)

class IsSing a where sing :: Sing a
instance IsSing 'Z where
    sing = SZ
instance IsSing (n :: INat) => IsSing ('S n) where
    sing = SS sing
type SINat (n :: INat) = Sing n

newtype Unit a = MkUnit () deriving Show

unit :: Unit a
unit = MkUnit ()

type family Prod (n :: INat) (a :: *) = r | r -> n a where
    Prod 'Z a = Unit a
    Prod ('S n) a = (a, Prod n a)

type family Div2 (n :: INat) where
    Div2 'Z = 'Z
    Div2 ('S 'Z) = 'Z
    Div2 ('S ('S n)) = 'S (Div2 n)

type family Add (n :: INat) (m :: INat) where
    Add 'Z n = n
    Add ('S n) m = 'S (Add n m)

toNat :: (SINat n) -> Int
toNat SZ     = 0
toNat (SS n) = 1 + toNat n

addn :: SINat n -> SINat m -> SINat (Add n m)
addn SZ     n = n
addn (SS n) m = SS (addn n m)

type family Mul (n :: INat) (m :: INat) where
    Mul 'Z n = 'Z
    Mul ('S n) m = Add m (Mul n m)

mul :: SINat n -> SINat m -> SINat (Mul n m)
mul SZ     n = SZ
mul (SS n) m = addn m (mul n m)

type family Pow2 (n :: INat) where
    Pow2 'Z = 'S 'Z
    Pow2 ('S n) = Add (Pow2 n) (Pow2 n)

pow2 :: SINat n -> SINat (Pow2 n)
pow2 SZ     = SS SZ
pow2 (SS n) = addn (pow2 n) (pow2 n)

type family ToNat (i :: INat) :: Nat where
    ToNat 'Z = 0
    ToNat ('S n) = 1 + ToNat n

type family FromNat (i :: Nat) :: INat where
    FromNat 0 = 'Z
    FromNat n = 'S (FromNat (n-1))

type family Tree (n :: INat) (a :: *) where
  Tree 'Z a = a
  Tree ('S n) a = (Tree n a, Tree n a)

szr :: SINat n -> Add n Z :~: n
szr SZ     = Refl
szr (SS n) = case szr n of
    Refl -> Refl

ssr :: SINat n -> SINat m -> Add n (S m) :~: S (Add n m)
ssr SZ     _ = Refl
ssr (SS n) m = case ssr n m of
    Refl -> Refl

addComm :: SINat n -> SINat m -> Add n m :~: Add m n
addComm SZ m = case szr m of
    Refl -> Refl -- unsafeCoerce Refl
addComm (SS n) m = case (addComm n m, ssr m n) of
    (Refl, Refl) -> Refl

fromINat :: forall a n . Num a => SINat n -> a
fromINat SZ     = 0
fromINat (SS x) = 1 + fromINat x

data SDict a where
    SDict :: Serialise a => SDict a

getDict :: Serialise a => SINat n -> Proxy a -> SDict (Tree n a)
getDict SZ     _ = SDict
getDict (SS n) p = case getDict n p of
    SDict -> SDict