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
module Language.Poly.Core
  ( Core(..)
  , Serialise
  , pattern IdF
  , pattern InlF
  , pattern InrF
  , pattern SplitF
  , pattern CaseF
  -- , pattern FstF
  -- , pattern SndF
  , interp
  , polyC
  , (:->)(..)
  , extractType
  , extractParamType
  ) where

import Prelude hiding ( id, (.), fst, snd, const, curry )

import Control.Constrained.Arrow
import Control.Constrained.Category
import Control.Constrained.ArrowVector
import Data.Singletons.TypeLits
import Data.Typeable
import Data.Type.Vector ( Vec )
import qualified Data.Type.Vector as Vec
import Data.Type.Mod
import Data.Text.Prettyprint.Doc ( Pretty(..) )
import Data.Text.Prettyprint.EDoc
import Language.Poly.Type
import Data.Constraint ( Dict(..) )
import CodeGen.Type

infixr 4 :->

type CC = Typeable

-- class Serialise a => Bcc a where
-- instance (Bcc a, Bcc b) => Bcc (a, b)
-- instance (Bcc a, Bcc b) => Bcc (Either a b)

type Serialise a = (Repr a, Show a, Read a, Typeable a)

class Show a => Value a where

instance Show a => Value a where

data Core (a :: *)
  where
    Unit  :: Core ()

    Lit :: Value a => a -> Core a

    -- use internally
    Var :: Integer -> Core a
    Val :: Value a => a -> Core a

    -- Apply
    (:$) :: (Serialise a) => Core (a -> b) -> Core a -> Core b
    -- (:$) :: a :-> b -> Core a -> Core b

    Prim  :: String -- XXX: name of the "primitive function"
          -> a -- Semantics in Haskell of the primitive function
          -> Core a

    Curry :: (CC a, CC b, CC c)
          => (a, b) :-> c
          -> Core (a -> b -> c)

    Ap :: Core ((a :-> b, a) -> b)

    Const :: Core a
          -> Core (b -> a)

    Id    :: Core (a -> a)

    Comp  :: (CC b, CC c, CC a)
          => b :-> c
          -> a :-> b
          -> Core (a -> c)

    Fst   :: (Serialise b, CC b, CC a)
          => Core ((a, b) -> a)

    Snd   :: (Serialise a, CC a, CC b)
          => Core ((a, b) -> b)
    
    Pair  :: (CC a, CC b)
          => Core a
          -> Core b
          -> Core (a, b)

    Split :: (CC a, CC b, CC c)
          => a :-> b
          -> a :-> c
          -> Core (a -> (b, c))

    Inl  :: (CC a, CC b)
          => Core (a -> Either a b)

    Inr  :: (CC a, CC b)
          => Core (b -> Either a b)

    Case ::(CC a, CC b, CC c)
         => b :-> a
         -> c :-> a
         -> Core (Either b c -> a)

    -- Vector operations isomorphic to products of n elements
    -- without type-safety: split = vect, proj = get
    Vect :: (CC a, CC b, KnownNat n)
         => (TMod n -> a :-> b)
         -> Core (a -> Vec n b)

    Get :: (CC a, KnownNat m)
        => TMod m
        -> Core (Vec m a -> a)

    Fmap  :: IsC CC (:->) f a b
          => SPoly f
          -> a :-> b
          -> Core (f :@: a -> f :@: b)

    In   :: Data f t
         => Core (f :@: t -> t)

    Out  :: Data f t
         => Core (t -> f :@: t)

    Rec  :: IsC CC (:->) f a b
         => SPoly f
         -> f :@: b :-> b
         -> a :-> f :@: a
         -> Core (a -> b)

polyC :: SPoly p -> Proxy a -> Proxy b -> Dict (IsC NoConstraint (->) p a b)
polyC FId _ _ = Dict
polyC (FK _) _ _ = Dict
polyC (FProd l r) a b =
  case (polyC l a b, polyC r a b) of
    (Dict, Dict) -> Dict
polyC (FSum l r) a b =
  case (polyC l a b, polyC r a b) of
    (Dict, Dict) -> Dict

newtype (:->) a b = Fun { repr :: Core (a -> b) }

pattern IdF :: forall i o. (CC i, CC o) => forall a. (i ~ a, o ~ a) => i :-> o
pattern IdF = Fun Id

pattern InlF :: forall i o. (CC i, CC o) => forall a b. (i ~ a, o ~ Either a b, CC a, CC b) => i :-> o
pattern InlF = Fun Inl

pattern InrF :: forall i o. (CC i, CC o) => forall a b. (i ~ a, o ~ Either b a, CC a, CC b) => i :-> o
pattern InrF = Fun Inr

pattern SplitF :: forall i o. (CC i, CC o) => forall a b c. (i ~ a, o ~ (b,c), CC b, CC c)
               => a :-> b -> a :-> c -> i :-> o
pattern SplitF a b = Fun (Split a b)

pattern CaseF :: forall i o. (CC i, CC o) => forall a b c. (i ~ Either a b, o ~ c, CC a, CC b)
              => a :-> c -> b :-> c -> i :-> o
pattern CaseF a b = Fun (Case a b)

-- pattern FstF :: forall i o. (CC i, CC o) => forall a b. (i ~ (a,b), o ~ a, CC a, CC b) => i :-> o
-- pattern FstF = Fun Fst

-- pattern SndF :: forall i o. (CC i, CC o) => forall a b. (i ~ (a,b), o ~ b, CC b, CC a) => i :-> o
-- pattern SndF = Fun Snd

ap :: (a -> b, a) -> b
ap (f, x) = f x

interp :: Core t -> t
interp Unit = ()
interp (Lit x) = x
interp (Val x) = x
interp (Prim _ f) = f
interp Ap = ap . \(f, x) -> (interp . repr $ f, x)
interp (Curry (repr -> f)) = curry (interp f)
interp (Const v) = const $ interp v
interp Id = id
interp (Comp (repr -> f) (repr -> g)) = interp f . interp g
interp Fst = fst
interp Snd = snd
interp (Split (repr -> f) (repr -> g)) = interp f &&& interp g
interp Inl = Left
interp Inr = Right
interp (Case (repr -> f) (repr -> g)) = interp f ||| interp g
interp (Vect nf) = Vec.vec interpnf
  where
    interpnf = interp . repr . nf
interp (Get i) = proj i
interp (Fmap p (repr -> f)) =
  case polyC p (dom f) (cod f) of
    Dict -> pmap p $ interp f
interp In = roll
interp Out = unroll
interp (Rec p (repr -> f) (repr -> g)) = h
  where
    h = ef . go h . eg
    ef = interp f
    eg = interp g
    go =
      case polyC p (dom g) (cod f) of
        Dict -> pmap p

dom :: Core (a -> b) -> Proxy a
dom _ = Proxy

cod :: Core (a -> b) -> Proxy b
cod _ = Proxy

extractType :: Core a -> Proxy a
extractType _ = Proxy

extractParamType :: (Core a -> b) -> Proxy a
extractParamType _ = Proxy

instance Value a => Const CC a (:->) where
  const = Fun . Const . Val

instance Category CC (:->) where
  id = Fun Id
  f . g = Fun (f `Comp` g)

instance Arrow CC (:->) where
  arr s f = Fun $ Prim s f
  -- fst = Fun Fst
  -- snd = Fun Snd
  (***) f g = Fun $ Split (f . fst) (g . snd)
  (&&&) f g = Fun $ Split f g
  first f = Fun $ Split (f . fst) snd
  second f = Fun $ Split fst (f . snd)

instance ArrowChoice CC (:->) where
  inl = Fun Inl
  inr = Fun Inr
  left f = Fun $ Case (inl . f) inr
  right f = Fun $ Case inl (inr . f)
  f +++ g = Fun $ Case (inl . f) (inr . g)
  f ||| g = Fun $ Case f g

instance ArrowVector CC Vec (:->) where
  proj = Fun . Get
  vec f = Fun (Vect f)

instance ArrowApply CC (:->) where
  app = Fun Ap

instance Pretty (a :-> b) where
  pretty (Fun f) = pretty f

instance Pretty (Core a) where
  pretty Unit = [ppr| "()" |]
  pretty (Val x) = [ppr| show x |]
  pretty (Lit x) = [ppr| show x |]
  pretty (Var x) = [ppr| "variable" + "(" + show x + ")"|]
  pretty (Prim s _) = [ppr| s |]
  pretty Ap = [ppr| "ap" |]
  pretty (Curry f) =  [ppr| "curry" + "(" > f > ")" |]
  pretty (Const v) = [ppr| "const" + "(" > v > ")" |]
  pretty Id =  [ppr| "id" |]
  pretty (Comp f g) = [ppr| f + "." + g |]
  pretty Fst = [ppr| "fst" |]
  pretty Snd = [ppr| "snd" |]
  pretty (Split f g) = [ppr| f + "&&&" + g |]
  pretty Inl = [ppr| "inl" |]
  pretty Inr = [ppr| "inr" |]
  pretty (Case f g) = [ppr| f + "|||" + g |]
  pretty (Vect _) = [ppr| "vec (<functions>)" |]
  pretty (Get i) = [ppr| "proj " > show i  |]
  pretty (Fmap p f) = [ppr| p + "(" > f > ")" |]
  pretty In = [ppr| "roll" |]
  pretty Out = [ppr| "unroll" |]
  pretty (Rec p f g) = [ppr| "rec" + p + "(" > f > ")" + "(" > g > ")" |]
