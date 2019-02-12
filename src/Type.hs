{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE DataKinds      #-}
module Type where

import           Def
import           Data.Typeable
import           Control.Monad.Free

data TProcF next where
    TSend :: RoleID -> a -> next -> TProcF next
    TReceive :: RoleID -> a -> next -> TProcF next
    TSelect :: [RoleID] -> next -> next -> TProcF next
    TBranch :: RoleID -> next -> next -> TProcF next

instance Functor TProcF where
    fmap f (TSend r e n)    = TSend r e $ f n
    fmap f (TReceive r a n) = TReceive r a $ f n
    fmap f (TBranch r a b)  = TBranch r (f a) (f b)
    fmap f (TSelect r a b)  = TSelect r (f a) (f b)

type TProc a = Free TProcF a
