{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module FFT where

import           Data.Proxy
import           GHC.TypeLits

import           Language.Poly.Core2
import           Language.Poly.Nat
import           ParPattern
import           CodeGen.Type
import           CodeGen

type Complex a = (a, a)

cbaseFFT :: Core ([Complex Float] -> [Complex Float])
cbaseFFT = Prim "baseFFT" undefined

cexp :: Core ((Int, Int) -> Complex Float)
cexp = Prim "exp" undefined

csplitList :: Core ([a] -> ([a], [a]))
csplitList = Prim "splitList" undefined

caddc :: Core (([Complex Float], [Complex Float]) -> [Complex Float])
caddc = Prim "addc" undefined

csubc :: Core (([Complex Float], [Complex Float]) -> [Complex Float])
csubc = Prim "subc" undefined

caddPadding :: SNat n -> Core ([a] -> [a])
caddPadding _ = Prim "addPadding" undefined

cconcatenate :: Core (([a], [a]) -> [a])
cconcatenate = Prim "concatenate" undefined

cmulexp :: Core (Int -> Int -> [Complex Float] -> [Complex Float])
cmulexp = Prim "cmulexp" undefined

-- core zone
csplitL :: SNat n -> Core ([Complex Float] -> Tree n [Complex Float])
csplitL SZ     = Id
csplitL (SS n) = case getSDict n (Proxy :: Proxy [Complex Float]) of
    SDict -> csplitList :>>> (csplitL n :*** csplitL n)

cmerge :: SNat n -> Core (Tree n [Complex Float] -> [Complex Float])
cmerge SZ     = Id
cmerge (SS n) = case getSDict n (Proxy :: Proxy [Complex Float]) of
    SDict -> (cmerge n :*** cmerge n) :>>> cconcatenate

czwT
    :: SNat n
    -> Core (([Complex Float], [Complex Float]) -> [Complex Float])
    -> Core
           (  (Tree n [Complex Float], Tree n [Complex Float])
           -> Tree n [Complex Float]
           )
czwT SZ     f = f
czwT (SS n) f = case getSDict n (Proxy :: Proxy [Complex Float]) of
    SDict -> Swap :>>> (czwT n f :*** czwT n f)

testf :: Core ((Int, Int) -> Int)
testf = Prim "test" undefined

testSwap :: Core (((a, b), (c, d)) -> ((a, c), (b, d)))
testSwap = Prim "swap" undefined

testzwT' :: SNat n -> Core ((Tree n Int, Tree n Int) -> Tree n Int)
testzwT' SZ     = testf
testzwT' (SS n) = case getSDict n (Proxy :: Proxy Int) of
    SDict -> testSwap :>>> (testzwT' n :*** testzwT' n)

testzwT
    :: forall n
     . (KnownNat n, SingI (FromNat n))
    => Core
           (  (Tree (FromNat n) Int, Tree (FromNat n) Int)
           -> Tree (FromNat n) Int
           )
testzwT = testzwT' (sing :: SNat (FromNat n))

testTree :: SNat n -> SingleType (Tree n [Complex Float])
testTree SZ     = singleType
testTree (SS x) = ProductSingleType (testTree x) (testTree x)

cfmapTIx
    :: SNat n
    -> Core (Int -> [Complex Float] -> [Complex Float])
    -> Int
    -> Core (Tree n [Complex Float] -> Tree n [Complex Float])
cfmapTIx SZ     f k = f :$ (Lit k)
cfmapTIx (SS x) f k = case getSDict x (Proxy :: Proxy [Complex Float]) of
    SDict -> cfmapTIx x f k :*** cfmapTIx x f (k + (2 ^ sNatToInt x))

cfft :: SNat n -> Core (Tree n [Complex Float] -> Tree n [Complex Float])
cfft SZ     = cbaseFFT
cfft (SS x) = case getSDict x (Proxy :: Proxy [Complex Float]) of
    SDict ->
        (cfft x :*** cfft x)
            :>>> (Id :*** cfmapTIx x (cmulexp :$ (Lit p2sx)) 0)
            :>>> (czwT x caddc :&&& czwT x csubc)
        where p2sx = 2 ^ sNatToInt (SS x)

cfastFourierR :: SNat n -> Core ([Complex Float] -> [Complex Float])
cfastFourierR cores = case getSDict cores (Proxy :: Proxy [Complex Float]) of
    SDict ->
        (    caddPadding cores
        :>>> csplitL cores
        :>>> cfft cores
        :>>> cmerge cores
        )

cfastFourier
    :: forall n
     . (KnownNat n, SingI (FromNat n))
    => Core ([Complex Float] -> [Complex Float])
cfastFourier = cfastFourierR (sing :: SNat (FromNat n))

cfft4Core :: ArrowPipe [Complex Float] [Complex Float]
cfft4Core = c2a $ (cfastFourier @1)

opt :: Core (a -> b) -> Core (a -> b)
opt ((a :>>> b) :>>> c) = opt $ (opt a :>>> (opt b :>>> opt c))
opt (((a :&&& b) :&&& (c :&&& d)) :>>> (Swap :>>> x)) =
    (((opt a :&&& opt c) :&&& (opt b :&&& opt d)) :>>> opt x)
opt (((a :&&& b) :&&& (c :&&& d)) :>>> Swap) =
    (((opt a :&&& opt c) :&&& (opt b :&&& opt d)))
-- opt ((e1 :*** e2) :>>> (e3 :*** e4)) = (e1 :>>> e3) :*** (e2 :>>> e4)
opt (x :>>> y) = opt x :>>> opt y
opt (x :*** y) = opt x :*** opt y
opt (x :&&& y) = opt x :&&& opt y
opt x          = x

c2a :: (Serialise a, Serialise b) => Core (a -> b) -> ArrowPipe a b
c2a (x :>>> y) = (c2a x) >>> (c2a y)
c2a (x :*** y) = (c2a x) *** (c2a y)
c2a (x :&&& y) = (c2a x) &&& (c2a y)
c2a x          = arr x

srcData2 :: (Tree ( 'S ( 'S 'Z)) Int, Tree ( 'S ( 'S 'Z)) Int)
srcData2 = (((1, 2), (3, 4)), ((5, 6), (7, 8)))

srcData1 :: (Tree ( 'S 'Z) Int, Tree ( 'S 'Z) Int)
srcData1 = ((1, 2), (3, 4))

codegenZWT = codeGenTest srcData1 (c2a (testzwT @1)) "codegen/test"