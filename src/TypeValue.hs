module TypeValue where

import Control.Monad.Free
import Data.Functor.Classes
import Data.Type.Natural (Nat)
import Data.Typeable
import Type hiding (project, dual)

type STypeV a = SType TypeRep a

project :: STypeV a -> Nat -> STypeV a
project (Pure a) _ = Pure a
project (Free (S r0 v next)) r1 | r0 == r1  = Free (S r0 v (project next r1))
                                | otherwise = project next r1
project (Free (R r0 v next)) r1 | r0 == r1  = Free (R r0 v (project next r1))
                                | otherwise = project next r1
project (Free (B r0 next1 next2 next)) r1
  | r0 == r1
  = Free (B r0 (project next1 r1) (project next2 r1) (project next r1))
  | otherwise
  = projectHelper (project next1 r1) (project next2 r1) >> project next r1
project (Free (Se r0 next1 next2 next)) r1
  | r0 == r1
  = Free (Se r0 (project next1 r1) (project next2 r1) (project next r1))
  | otherwise
  = projectHelper (project next1 r1) (project next2 r1) >> project next r1

projectHelper :: STypeV a -> STypeV a -> STypeV a
projectHelper a b = if liftEq (\_ _ -> True) a b
  then a
  else error "Projectition rule failure in branch or select"

dual :: STypeV a -> Nat -> STypeV a
dual (Pure b            ) _  = Pure b
dual (Free (S _r0 v next)) r1 = Free (R r1 v (dual next r1))
dual (Free (R _r0 v next)) r1 = Free (S r1 v (dual next r1))
dual (Free (B _r0 next1 next2 next)) r1 =
  Free (Se r1 (dual next1 r1) (dual next2 r1) (dual next r1))
dual (Free (Se _r0 next1 next2 next)) r1 =
  Free (B r1 (dual next1 r1) (dual next2 r1) (dual next r1))

dualityC :: Eq c => [(STypeV c, Nat)] -> Bool
dualityC = and . fmap (uncurry isDualHelper) . handShake
 where
  handShake []       = []
  handShake (x : xs) = fmap ((,) x) xs ++ handShake xs
  isDualHelper (a, aid) (b, bid) =
    dual (project a bid) aid
      == project b aid
      && dual (project b aid) bid
      == project a bid
