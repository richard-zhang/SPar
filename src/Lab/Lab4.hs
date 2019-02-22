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
module Lab.Lab4 where

import           Data.Type.Equality
import           Data.Proxy
import           Prelude                 hiding ( Monad(..) )

type family (++) (a :: [k]) (b :: [k]) :: [k] where
    '[] ++ l = l
    (e ': l) ++ l' = e ': l ++ l'

class IxFunctor f where
    imap :: (a -> b) -> f j k a -> f j k b

class IxFunctor m => IxMonad (m :: ix -> ix -> * -> *) where
    return :: a -> m i i a
    (>>=) :: m i j a -> (a -> m j k b) -> m i k b
    (>>) :: m i j a -> m j k b -> m i k b
    a >> b = a >>= const b
    fail :: String -> m i j a
    fail = error

data IxFree f i j a where
    Pure :: a -> IxFree f i i a
    Free :: f i j (IxFree f j k a) -> IxFree f i k a

instance IxFunctor f => Functor (IxFree f i j) where
    fmap f (Pure a)  = Pure (f a)
    fmap f (Free fa) = Free (imap (fmap f) fa)

instance IxFunctor f => IxFunctor (IxFree f) where
    imap = fmap

instance IxFunctor f => IxMonad (IxFree f) where
    return = Pure
    Pure a  >>= f = f a
    Free fa >>= f = Free (imap (>>= f) fa)

data ActionF i j next where
    Input :: (a -> next) -> ActionF (a ': i) i next
    Output :: String -> next -> ActionF i i next

instance Functor (ActionF i j) where
    fmap f (Input cont) = Input (f . cont)
    fmap f (Output v n) = Output v (f n)

instance IxFunctor ActionF where
    imap = fmap

liftf :: IxFunctor f => f i j a -> IxFree f i j a
liftf = Free . imap Pure

type Action' xs rest = IxFree ActionF (xs ++ rest) rest
type Action xs a = forall rest. IxFree ActionF (xs ++ rest) rest a

input :: IxFree ActionF (a : j) j a
input = liftf (Input id)

output' :: String -> Action '[] ()
output' s = liftf (Output s ())

addTwice :: IxFree ActionF (Int : String : j) j ()
addTwice = do
    (x :: Int   ) <- input
    (y :: String) <- input
    output' (show x)

eval :: Action' xs r a -> HList xs -> [String]
eval (Pure  a              ) xs         = []
eval (Free (Input k     )) (x ::: xs) = eval (k x) xs
eval (Free (Output s nxt)) xs         = s : eval nxt xs

data HList i where
    HNil :: HList '[]
    (:::) :: h -> HList t -> HList (h ': t)