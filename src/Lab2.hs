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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lab2 where

import Data.Type.Equality
import Data.Proxy
import Prelude hiding (Monad(..))

data HList i where
    HNil :: HList '[]
    (:::) :: h -> HList t -> HList (h ': t)

infixr 5 :::

type family (++) (a :: [k]) (b :: [k]) :: [k] where
    '[] ++ l = l
    (e ': l) ++ l' = e ': l ++ l'

class Functor1 (f :: k -> * -> *) where
    fmap1 :: (a -> b) -> f i a -> f i b

-- Adding additional index type i to capture type level information 
-- about the requirements of the methods
data ActionF i next where
    Input :: (a -> next) -> ActionF '[a] next
    Output :: String -> next -> ActionF '[] next

instance Functor (ActionF i) where 
    fmap f (Input c) = Input (fmap f c)
    fmap f (Output s n) = Output s (f n)

instance Functor1 (ActionF :: [*] -> * -> *)where
    fmap1 = fmap

data FreeIx (f :: [k] -> * -> *) (i :: [k]) a where
    Return :: a -> FreeIx f '[] a
    Free :: (WitnessList i) => f i (FreeIx f j a) -> FreeIx f (i ++ j) a

instance (Functor1 f) => Functor (FreeIx f i) where 
    fmap f (Return a) = Return (f a)
    fmap f (Free x) = Free (fmap1 (fmap f) x)

instance (Functor1 f) => Functor1 (FreeIx f) where
    fmap1 = fmap

-- if you want FreeIx live with an ordinary, unindexed functor 
-- data IxIdentityT f i a = IxIdentityT {runIxIdentityT :: f a}

data SList (i :: [k]) where
    SNil :: SList '[]
    SSucc :: SList t -> SList (h ': t)

appAssoc :: SList xs -> Proxy ys -> Proxy zs -> (xs ++ (ys ++ zs)) :~: ((xs ++ ys) ++ zs)
appAssoc SNil ys zs = Refl
appAssoc (SSucc xs) ys zs = case appAssoc xs ys zs of Refl -> Refl

appRightId :: SList xs -> xs :~: (xs ++ '[])
appRightId SNil = Refl
appRightId (SSucc xs) = case appRightId xs of Refl -> Refl

class WitnessList (xs :: [k]) where
    witness :: SList xs

instance WitnessList '[] where
    witness = SNil

instance WitnessList xs => WitnessList (x ': xs) where
    witness = SSucc witness

-- The explicit forall is for ScopedTypeVariables
liftF :: forall f i a . (WitnessList i, Functor1 f) => f i a -> FreeIx f i a
liftF = case appRightId (witness :: SList i) of Refl -> Free . fmap1 Return

type Action i a = FreeIx ActionF i a

input :: Action '[a] a
input = liftF (Input id)

output :: String -> Action '[] ()
output s = liftF (Output s ())

-- class Functor1 m => IxMonad (m :: k -> * -> *) where
--     type Unit :: k
--     type Plus (i :: k) (j :: k) :: k

--     return :: a -> m Unit a
--     (>>=) :: m i a -> (a -> m j b) -> m (Plus i j) b

--     (>>) :: m i a -> m j b -> m (Plus i j) b
--     a >> b = a >>= const b

--     fail :: String -> m i a
--     fail = error

bind :: forall f i j a b . (Functor1 f) => FreeIx f i a -> (a -> FreeIx f j b) -> FreeIx f (i ++ j) b 
bind (Return a) f = f a
bind (Free (x :: f i1 (FreeIx f j1 a))) f =
    case appAssoc (witness :: SList i1) (Proxy :: Proxy j1) (Proxy :: Proxy j)
    of Refl -> Free (fmap1 (`bind` f) x)

-- instance (Functor1 f) => IxMonad (FreeIx f) where
--     type Unit = '[]
--     type Plus i j = i ++ j

--     return = Return
--     (>>=) = bind

class IxFunctor (f :: ix -> ix -> * -> *) where
    imap :: (a -> b) -> f i j a -> f i j b

class IxFunctor m => IxMonad (m :: ix -> ix -> * -> *) where
    return :: a -> m i i a
    (>>=) :: m j k a -> (a -> m i j b) -> m i k b
    (>>) :: m j k a -> m i j b -> m i k b
    a >> b = a >>= const b
    fail :: String -> m i j a
    fail = error

data IxFree f i j a where
    Pure :: a -> IxFree f i i a
    Free' :: f j k (IxFree f i j a) -> IxFree f i k a -- compute bottom-up
    
instance IxFunctor f => Functor (IxFree f i j) where
    fmap f (Pure a)  = Pure (f a)
    fmap f (Free' fa) = Free' (imap (fmap f) fa)

instance IxFunctor f => IxFunctor (IxFree f) where
    imap = fmap
    
instance IxFunctor f => IxMonad (IxFree f) where
    return = Pure
    Pure a  >>= f = f a
    Free' fa >>= f = Free' (imap (>>= f) fa)
    
liftf :: IxFunctor f => f i j a -> IxFree f i j a
liftf = Free' . imap Pure

data ActionF' i j next where
    Input' :: (a -> next) -> ActionF' i (a ': i) next
    Output' :: String -> next -> ActionF' i i next

deriving instance Functor (ActionF' i j)

instance IxFunctor ActionF' where
    imap = fmap

type Action'' xs rest = IxFree ActionF' rest (xs ++ rest)
type Action' xs a = forall rest. IxFree ActionF' rest (xs ++ rest) a

input' :: Action' '[a] a
input' = liftf (Input' id)

output' :: String -> Action' '[] ()
output' s = liftf (Output' s ())

eval :: Action'' xs r a -> HList xs -> [String]
eval (Pure a) xs = []
eval (Free' (Input' k)) (x ::: xs) = eval (k x) xs
eval (Free' (Output' s nxt)) xs = s : eval nxt xs
