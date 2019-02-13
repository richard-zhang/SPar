{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
module Lab where

import           Control.Monad.Free
import           Data.Singletons
import           Type.Reflection
import Data.Singletons.TypeRepStar
import           Data.Kind

type CC = Typeable

data Mp next where
    S :: a -> next -> Mp next
    R :: a -> next -> Mp next

instance Functor Mp where
    fmap f (S a n) = S a $ f n
    fmap f (R a n) = R a $ f n

type M a = Free Mp a

type family (>*>) (a :: M c) (b :: M c) :: M c where
    'Free ('S v n) >*> b = 'Free ('S v (n >*> b))
    'Free ('R v n) >*> b = 'Free ('R v (n >*> b))
    'Pure _ >*> b = b

data MyType (ty :: Type) where
    MyType :: Typeable ty => MyType ty

data instance Sing (t :: Mp Type) where
    FSend :: MyType a -> Sing b -> Sing ('S a b :: Mp Type)
    FRecv :: MyType a -> Sing b -> Sing ('R a b :: Mp Type)

data instance Sing (t :: M Type) where
    FPure :: Proxy a -> Sing ('Pure a :: M Type)
    FreeS :: Proxy a -> Sing (n :: M Type) -> Sing ('Free ('S a n):: M Type)
    FreeR :: Proxy a -> Sing (n :: M Type) -> Sing ('Free ('R a n):: M Type)
    -- FPure' :: Sing a -> Sing ('Pure a :: M Type)
    -- FreeS' :: Sing a -> Sing (n :: M Type) -> Sing ('Free ('S a n):: M Type)
    -- FreeR' :: Sing a -> Sing (n :: M Type) -> Sing ('Free ('R a n):: M Type)

combine :: Sing (a :: M Type) -> Sing (b :: M Type) -> Sing (a >*> b :: M Type)
combine (FPure _)   y = y
combine (FreeS a b) y = FreeS a $ combine b y
combine (FreeR a b) y = FreeR a $ combine b y

a = FPure $ (Proxy :: Proxy Int)
b = FreeS (Proxy :: Proxy Int) a
c = combine a b

-- typeCheck :: M () -> Sing (t :: M Type)
-- typeCheck (Pure _) = undefined
type SM (t :: M Type) = Sing t

-- typeInfer :: M () -> Sing (t :: M Type)

-- typeCheck :: SM p -> M a -> Bool
-- typeCheck (FPure _) (Pure _) = True
-- typeCheck (FreeS proxy value) (Free (S a n)) = undefined 
-- typeCheck _ _ = False

toProxy :: a -> Proxy a
toProxy _ = Proxy

-- toSM :: Proxy a -> Proxy ( 'Free ( 'S a n))
-- toSM _ = Proxy

-- toRM :: Proxy a -> Proxy ( 'Free ( 'R a n))
-- toRM _ = Proxy

-- typeCheck :: M a -> forall c . Proxy (c :: M Type)
-- typeCheck (Free (S a n)) = undefined
--   where
--     s    = toSM $ toProxy a
--     cont = typeCheck n
-- typeCheck (Pure _) = Proxy :: Proxy ('Pure ())

-- typeCheck (Pure _) = Proxy :: Proxy ('Pure () :: M Type)

-- data Nat =
--     Zero
--     | Succ Nat

-- data Wrapper :: Nat -> * where
--     SingBase :: Wrapper (c :: Nat)
--     SingSuss :: Wrapper (c :: Nat) -> Wrapper ('Succ c :: Nat)

-- toWrapperInt :: Int -> Wrapper (c :: Nat)
-- toWrapperInt 0 = SingBase :: Wrapper ('Zero)
-- toWrapperInt a = SingSuss $ toWrapperInt $ a - 1