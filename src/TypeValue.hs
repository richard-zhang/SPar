module TypeValue where

import           Type
import           Control.Monad.Free
import           GHC.Natural
import           Data.Typeable
import Data.Functor.Classes

type STypeV a = SType TypeRep a

project :: STypeV a -> Natural -> STypeV a
project (Pure a) _ = Pure a
project (Free (S r0 v next)) r1
  | r0 == fromIntegral r1 = Free (S r0 v (project next r1))
  | otherwise             = project next r1
project (Free (R r0 v next)) r1
  | r0 == fromIntegral r1 = Free (R r0 v (project next r1))
  | otherwise             = project next r1
project (Free (B r0 next1 next2 next)) r1
  | r0 == fromIntegral r1 = Free
    (B r0 (project next1 r1) (project next2 r1) (project next r1))
  | otherwise = projectHelper (project next1 r1) (project next2 r1) >> project next r1 
project (Free (Se r0 next1 next2 next)) r1
  | r0 == fromIntegral r1 = Free
    (Se r0 (project next1 r1) (project next2 r1) (project next r1))
  | otherwise = projectHelper (project next1 r1) (project next2 r1) >> project next r1 

projectHelper :: STypeV a -> STypeV a -> STypeV a
projectHelper a b = if liftEq (\_ _ -> True) a b
  then a
  else error "Projectition rule failure in branch or select"

-- dual :: STypeV a -> Natural -> STypeV a 
-- dual (Pure b) _ = Pure b
-- dual (Free (S r0 v next)) r1 = Free (R r1 v (Dual next r1))
-- dual (Free (R r0 v next)) r1 = Free (S r1 v (dual next r1))
-- dual (Free (B r0 next1 next2 next)) r1 = Free (Se r1 (dual next1 r1) (dual next2 r1) (dual next r1))
-- dual (Free (Se r0 next1 next2 next)) r1 = Free (B r1 (dual next1 r1) (dual next2 r1) (dual next r1))

proc1 :: STypeV ()
proc1 = Free (S 1 (typeOf "str") (Pure ()))

proc2 :: STypeV ()
proc2 = Free (S 1 (typeOf 'c') (Pure ()))

proc3 :: STypeV ()
proc3 = Free (R 1 (typeOf "str") (Pure ()))