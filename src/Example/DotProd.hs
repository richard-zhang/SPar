{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE KindSignatures            #-}
module Example.DotProd where
import           Lib

imult :: Core ((Int, Int) -> Int)
imult = Prim "imult" (uncurry (*))

isum :: Core ((Int, Int) -> Int)
isum = Prim "isum" (uncurry (+))

splitv
    :: Core
           (  ([Int], [Int])
           -> Either (Either () (int, int)) (([Int], [Int]), ([Int], [Int]))
           )
splitv = Prim "splitv" undefined

baseFunc :: Core (([Int], [Int]) -> Int)
baseFunc = Prim "dotp" undefined

interleave
    :: (Serialise a, Serialise b, Serialise c, Serialise d)
    => ArrowPipe ((a, b), (c, d)) ((a, c), (b, d))
interleave = arr $ Prim "interleave" undefined

interleave2
    :: (Serialise a, Serialise b, Serialise c, Serialise d)
    => ArrowPipe ((a, b), (c, d)) ((a, c), (b, d))
interleave2 =
    ((arr Fst >>> arr Fst) &&& (arr Snd >>> arr Fst))
        &&& ((arr Fst >>> arr Snd) &&& (arr Snd >>> arr Snd))

-- fun interleave : forall a b c d, (a * b) * (c * d) -> (a * c) * (b * d)
--   =  ((proj[0,2] . proj[0,2]) &&& (proj[0,2] . proj[1,2]))
--  &&& ((proj[1,2] . proj[0,2]) &&& (proj[1,2] . proj[1,2]));

const0 :: Core (b -> Int)
const0 = Const (Lit 0)

testDotProd = runPipe1 zero (dotProd 1)

dotProd :: Int -> ArrowPipe ([Int], [Int]) Int
dotProd 0 = arr baseFunc
dotProd x =
    (arr splitv)
        >>> (   arr Inl
            ||| (   interleave
                >>> (   (arr Fst >>> dotProd (x - 1))
                    &&& (arr Snd >>> dotProd (x - 1))
                    )
                >>> arr Inr
                )
            )
        >>> ((arr const0 ||| arr imult) ||| arr isum)
