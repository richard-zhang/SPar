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
module Lab3 where

import           Data.Type.Equality
import           Data.Proxy
import           Prelude                 hiding ( Monad(..) )
import           Data.Singletons.TypeLits
import           Control.Monad.Free
import           Data.Kind

data Expr a where
    Var :: a -> Expr a
    End :: Expr ()

data Pf i next where
    Send :: Sing (n :: Nat) -> Expr a -> next -> Pf ('Free ('TS n a ('Pure ()))) next
    Recv :: Sing (n :: Nat) -> (Expr a -> next) -> Pf ('Free ('TR n a ('Pure ()))) next

data TPf next where
    TS :: Nat -> a -> next -> TPf next
    TR :: Nat -> a -> next -> TPf next

type TypeP a = Free TPf a

type family (>*>) (a :: TypeP c) (b :: TypeP c) :: TypeP c where
    'Free ('TS v r n) >*> b = 'Free ('TS v r (n >*> b))
    'Free ('TR v r n) >*> b = 'Free ('TR v r (n >*> b))
    'Pure _ >*> b = b

class IxFunctor (f :: k -> * -> *) where
    imap :: (a -> b) -> f i a -> f i b

instance Functor (Pf i) where
    fmap f (Send a v n) = Send a v $ f n
    fmap f (Recv a cont) = Recv a (f . cont)

instance IxFunctor Pf where
    imap = fmap

data FreeIx f (i :: TypeP *) a where
    Return :: a -> FreeIx f ('Pure ()) a
    Wrap :: (WitnessTypeP i)  => f i (FreeIx f j a) -> FreeIx f (i >*> j) a
    -- Wrap :: f i (FreeIx f j a) -> FreeIx f (i >*> j) a

instance (IxFunctor f) => Functor (FreeIx f i) where
    fmap f (Return a) = Return (f a)
    fmap f (Wrap x) = Wrap (imap (fmap f) x)

instance (IxFunctor f) => IxFunctor (FreeIx f) where
    imap = fmap

data STypeP (i :: TypeP k) where
    SPure :: STypeP ('Pure ())
    STs :: STypeP a -> STypeP ('Free ('TS c b a))
    STr :: STypeP a -> STypeP ('Free ('TR c b a))

class WitnessTypeP (i :: TypeP k) where
    witness :: STypeP i

instance WitnessTypeP ('Pure ()) where
    witness = SPure

instance WitnessTypeP a => WitnessTypeP ('Free ('TS c b a)) where
    witness = STs witness

instance WitnessTypeP a => WitnessTypeP ('Free ('TR c b a)) where
    witness = STr witness


appRightId :: STypeP i -> i :~: (i >*> 'Pure ())
appRightId SPure   = Refl
appRightId (STs a) = case appRightId a of
    Refl -> Refl
appRightId (STr a) = case appRightId a of
    Refl -> Refl

appAssoc
    :: STypeP x -> Proxy y -> Proxy z -> (x >*> (y >*> z)) :~: ((x >*> y) >*> z)
appAssoc SPure   y z = Refl
appAssoc (STs a) y z = case appAssoc a y z of
    Refl -> Refl
appAssoc (STr a) y z = case appAssoc a y z of
    Refl -> Refl

liftF' :: forall f i a . (WitnessTypeP i, IxFunctor f) => f i a -> FreeIx f i a
liftF' = case appRightId (witness :: STypeP i) of
    Refl -> Wrap . imap Return

type P i a = FreeIx Pf i (Expr a)

send :: Sing n -> Expr a -> FreeIx Pf ( 'Free ( 'TS n a ( 'Pure ()))) (Expr a)
send role value = liftF' (Send role value value)

recv :: Sing n -> FreeIx Pf ( 'Free ( 'TR n a ( 'Pure ()))) (Expr a)
recv role = liftF' (Recv role id)

class IxFunctor m => IxMonad (m :: k -> * -> *) where
    type Unit :: k
    type Plus (i :: k) (j :: k) :: k

    return :: a -> m Unit a
    (>>=) :: m i a -> (a -> m j b) -> m (Plus i j) b

    (>>) :: m i a -> m j b -> m (Plus i j) b
    a >> b = a >>= const b

    fail :: String -> m i a
    fail = error

bind
    :: forall f i j a b
     . IxFunctor f
    => FreeIx f i a
    -> (a -> FreeIx f j b)
    -> FreeIx f (i >*> j) b
bind (Return a) f = f a
bind (Wrap (x :: f i1 (FreeIx f j1 a))) f =
    case
            appAssoc (witness :: STypeP i1)
                     (Proxy :: Proxy j1)
                     (Proxy :: Proxy j)
        of
            Refl -> Wrap (imap (`bind` f) x)

instance (IxFunctor f) => IxMonad (FreeIx f) where
    type Unit = 'Pure ()
    type Plus i j = i >*> j

    return = Return
    (>>=) = bind

data Process (k :: (TypeP c, Nat)) where
    Process :: Sing (a :: Nat) -> P c b -> Process '(c, a)

-- test :: P ('Free ('TS 1 Integer ('Free ('TR 2 Integer ('Pure ()))))) ()
-- test :: P ('Free ('TS 1 Integer ('Free ('TR 2 Integer ('Pure ()))))) ()
test :: FreeIx Pf (Plus ('Free ('TS 1 Integer ('Pure ()))) ('Free ('TR 2 Integer ('Pure ())))) (Expr ())
test = do
    send (SNat :: Sing 1) (Var 10)
    _x :: Expr Integer <- recv (SNat :: Sing 2)
    return End

type family Project (a :: TypeP c) (r :: Nat) :: TypeP c where
    Project ('Pure b) _ = ('Pure b)
    Project ('Free ('TS r0 v next)) r0 = 'Free ('TS r0 v (Project next r0))
    Project ('Free ('TR r0 v next)) r0 = 'Free ('TR r0 v (Project next r0))
    Project ('Free ('TS r0 v next)) r1 = Project next r1
    Project ('Free ('TR r0 v next)) r1 = Project next r1

type family Dual (a :: TypeP c) (r :: Nat) :: TypeP c where
    Dual ('Pure b) _ = ('Pure b)
    Dual ('Free ('TS r0 v next)) r1 = 'Free ('TR r1 v (Dual next r1))
    Dual ('Free ('TR r0 v next)) r1 = 'Free ('TS r1 v (Dual next r1))

type family IsDualHelper (k1 :: (TypeP c, Nat)) (k2 :: (TypeP c, Nat)) :: Constraint where
    IsDualHelper '(a, aid) '(b, bid) = (Dual (Project a bid) aid ~ Project b aid, Dual (Project b aid) bid ~ Project a bid)

type family IsDual (k1 :: ((TypeP c, Nat), (TypeP c, Nat))) where
    IsDual '(k1, k2) = IsDualHelper k1 k2

type And (a :: Constraint) (b :: Constraint)  = (a, b)

type family AppendTop (a :: k1) (b :: [k2]) where
    AppendTop a '[] = '[]
    AppendTop a (x ': xs) = '(a, x) : AppendTop a xs

type family (++) (a :: [k]) (b :: [k]) where
    '[] ++ b = b
    (x ': xs) ++ b = x ': (xs ++ b)

type family Handshake (c :: [k]) where
    Handshake '[] = '[]
    Handshake (x ': xs) = AppendTop x xs ++ Handshake xs

type family DualityCHelper (c :: [((TypeP b, Nat), (TypeP b, Nat))]) :: Constraint where
    DualityCHelper '[] = ()
    DualityCHelper (x ': xs) = IsDual x `And` DualityCHelper xs

type family DualityC (c :: [(TypeP b, Nat)]) :: Constraint where
    DualityC xs = DualityCHelper (Handshake xs)

data HList (l::[*]) where
    HNil  :: HList '[]
    HCons :: e -> HList l -> HList (e ': l)

eval3
    :: DualityC '[info1, info2, info3]
    => Process info1
    -> Process info2
    -> Process info3
    -> [String]
eval3 = undefined
