{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeInType     #-}
{-# LANGUAGE DataKinds      #-}
module Lab where

import           Control.Monad.Free
import           Data.Kind


data Mp next where
    S :: a -> next -> Mp next
    R :: a -> next -> Mp next

instance Functor Mp where
    fmap f (S a n) = S a $ f n
    fmap f (R a n) = R a $ f n

type M a = Free Mp a

type family (>*>) (a :: Free Mp c) (b :: Free Mp c) :: Free Mp c where
    'Free ('S v n) >*> b = 'Free ('S v (n >*> b))
    'Free ('R v n) >*> b = 'Free ('R v (n >*> b))
    'Pure _ >*> b = b