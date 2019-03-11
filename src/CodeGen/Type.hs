{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeApplications #-}
module CodeGen.Type where

import Data.Bits
import Foreign.Storable                                             ( Storable )

data SingleType a where
    NumSingleType :: NumType a -> SingleType a

data NumType a where
    IntegralNumType :: IntegralType a -> NumType a
    FloatingNumType :: FloatingType a -> NumType a

data IntegralType a where
    TypeInt     :: IntegralDict Int     -> IntegralType Int

data FloatingType a where
    TypeFloat   :: FloatingDict Float   -> FloatingType Float


data IntegralDict a where
    IntegralDict :: ( Bounded a, Eq a, Ord a, Show a
                    , Bits a, FiniteBits a, Integral a, Num a, Real a, Storable a )
                    => IntegralDict a
    
data FloatingDict a where
    FloatingDict :: ( Eq a, Ord a, Show a
                    , Floating a, Fractional a, Num a, Real a, RealFrac a
                    , RealFloat a, Storable a )
                    => FloatingDict a

typeInt :: IntegralType Int
typeInt = TypeInt (IntegralDict @Int)

numTypeInt :: NumType Int
numTypeInt = IntegralNumType typeInt

singleTypeInt :: SingleType Int
singleTypeInt = NumSingleType numTypeInt