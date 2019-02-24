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
module Type where

import           Data.Type.Equality
import           Data.Proxy
import           Prelude                 hiding ( Monad(..) )
import           Data.Singletons.TypeLits
import           Control.Monad.Free
import           Data.Kind
import qualified Data.Map.Strict               as Map
import           GHC.Natural
import           Language.Poly.Core             ( Core(..) )
import qualified GHC.TypeLits

data STypeF next where
    S :: Nat -> a -> next -> STypeF next
    R :: Nat -> a -> next -> STypeF next
    B :: Nat -> SType c -> SType c -> next -> STypeF next
    Se :: Nat -> SType c -> SType c -> next -> STypeF next

type SType a = Free STypeF a

type family (>*>) (a :: SType c) (b :: SType c) :: SType c where
    'Free ('S r v n) >*> b = 'Free ('S r v (n >*> b))
    'Free ('R r v n) >*> b = 'Free ('R r v (n >*> b))
    'Free ('B r n1 n2 n3) >*> b = 'Free ('B r n1 n2 (n3 >*> b))
    'Free ('Se r n1 n2 n3) >*> b = 'Free ('Se r n1 n2 (n3 >*> b))
    'Pure _ >*> b = b

type family Project (a :: SType c) (r :: Nat) :: SType c where
    Project ('Pure b) _ = ('Pure b)
    Project ('Free ('S r0 v next)) r0 = 'Free ('S r0 v (Project next r0))
    Project ('Free ('R r0 v next)) r0 = 'Free ('R r0 v (Project next r0))
    Project ('Free ('B r0 next1 next2 next)) r0 = 'Free ('B r0 (Project next1 r0) (Project next2 r0) (Project next r0))
    Project ('Free ('Se r0 next1 next2 next)) r0 = 'Free ('Se r0 (Project next1 r0) (Project next2 r0) (Project next r0))
    Project ('Free ('S r0 v next)) r1 = Project next r1
    Project ('Free ('R r0 v next)) r1 = Project next r1
    Project ('Free ('B r0 next1 next2 next)) r1 = ProjectHelper (Project next1 r1) (Project next2 r1) >*> Project next r1
    Project ('Free ('Se r0 next1 next2 next)) r1 = ProjectHelper (Project next1 r1) (Project next2 r1) >*> Project next r1

type family ProjectHelper (left :: SType c) (right :: SType c) :: SType c where
    ProjectHelper a a = a
    ProjectHelper a b = GHC.TypeLits.TypeError ('GHC.TypeLits.Text "doesn't match in branches")

type family Dual (a :: SType c) (r :: Nat) :: SType c where
    Dual ('Pure b) _ = ('Pure b)
    Dual ('Free ('S r0 v next)) r1 = 'Free ('R r1 v (Dual next r1))
    Dual ('Free ('R r0 v next)) r1 = 'Free ('S r1 v (Dual next r1))
    Dual ('Free ('B r0 next1 next2 next)) r1 = 'Free ('Se r1 (Dual next1 r1) (Dual next2 r1) (Dual next r1))
    Dual ('Free ('Se r0 next1 next2 next)) r1 = 'Free ('B r1 (Dual next1 r1) (Dual next2 r1) (Dual next r1))

type family IsDualHelper (k1 :: (SType c, Nat)) (k2 :: (SType c, Nat)) :: Constraint where
    IsDualHelper '(a, aid) '(b, bid) = (Dual (Project a bid) aid ~ Project b aid, Dual (Project b aid) bid ~ Project a bid)

type family IsDual (k1 :: ((SType c, Nat), (SType c, Nat))) :: Constraint where
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

type family DualityCHelper (c :: [((SType b, Nat), (SType b, Nat))]) :: Constraint where
    DualityCHelper '[] = 'Pure () ~ 'Pure ()
    DualityCHelper (x ': xs) = IsDual x `And` DualityCHelper xs

type family DualityC (c :: [(SType b, Nat)]) :: Constraint where
    DualityC xs = DualityCHelper (Handshake xs)