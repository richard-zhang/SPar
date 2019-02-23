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
module Rt where

import           Prelude
import           Data.Type.Equality
import           Data.Proxy
import           Data.Singletons.TypeLits
import           Data.Singletons
import           Control.Monad.Free
import           Data.Kind
import qualified Data.Map.Strict               as Map
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           GHC.Natural
import           Data.Typeable
import           Language.Poly.Core             ( Core(Lit) )
import           Def
import Type

type ProcessRT a = (ProcRT a, Natural)

data ProcRTF next where
    Send' :: (CC a) => Natural -> Core a -> next -> ProcRTF next
    Recv' :: (CC a) => Natural -> (Core a -> next) -> ProcRTF next
    Select' :: (CC a, CC b, CC c) => Natural -> Core (Either a b) -> (Core a -> ProcRT c) -> (Core b -> ProcRT c) -> next -> ProcRTF next
    Branch' :: (CC c) => Natural -> ProcRT c -> ProcRT c -> next -> ProcRTF next

instance Functor ProcRTF where
    fmap f (Send' r v n) = Send' r v $ f n
    fmap f (Recv' r cont) = Recv' r (f . cont)
    fmap f (Select' r v cont1 cont2 n) = Select' r v cont1 cont2 (f n)
    fmap f (Branch' r p1 p2 n) = Branch' r p1 p2 (f n)

type ProcRT a = Free ProcRTF (Core a)

type GlobalMq = Map.Map Natural [String]
type Gs = StateT GlobalMq (WriterT [ObservableAction] IO) ()

data ObservableAction =
    ASend Natural Natural String
  | ARecv Natural Natural String
  | ASelect
  | ABranch
  deriving (Show)
