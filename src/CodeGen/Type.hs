{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module CodeGen.Type where

import Data.Bits
import Data.Typeable
import Foreign.Storable                                             ( Storable )

data SingleType a where
    NumSingleType :: NumType a -> SingleType a
    LabelSingleType :: SingleType Label
    UnionSingleType :: (Typeable a, Typeable b) => SingleType a -> SingleType b -> SingleType (Either a b)
    UnitSingleType :: SingleType ()
    ProductSingleType :: (Typeable a, Typeable b) => SingleType a -> SingleType b -> SingleType (a, b)

equal :: SingleType a -> SingleType b -> Bool
equal (NumSingleType (IntegralNumType _)) (NumSingleType (IntegralNumType _)) = True
equal LabelSingleType LabelSingleType = True
equal (UnionSingleType a b) (UnionSingleType a' b') = equal a a' && equal b b'
equal (ProductSingleType a b) (UnionSingleType a' b') = equal a a' && equal b b'
equal UnitSingleType UnitSingleType = True
equal _ _ = False

instance Eq (SingleType a) where
    (==) = equal

instance Ord (SingleType a) where 
    compare _ _ = LT

data NumType a where
    IntegralNumType :: IntegralType a -> NumType a
    FloatingNumType :: FloatingType a -> NumType a

data IntegralType a where
    TypeInt     :: IntegralDict Int     -> IntegralType Int

data FloatingType a where
    TypeFloat   :: FloatingDict Float   -> FloatingType Float


data IntegralDict a where
    IntegralDict :: ( Typeable a, Bounded a, Eq a, Ord a, Show a
                    , Bits a, FiniteBits a, Integral a, Num a, Real a, Storable a )
                    => IntegralDict a
    -- IntegralDict :: ( Bounded a, Eq a, Ord a, Show a
                    -- , Bits a, FiniteBits a, Integral a, Num a, Real a, Storable a )
                    -- => IntegralDict a
    
data FloatingDict a where
    FloatingDict :: ( Typeable a, Eq a, Ord a, Show a
                    , Floating a, Fractional a, Num a, Real a, RealFrac a
                    , RealFloat a, Storable a )
                    => FloatingDict a

data Label = Le | Ri
    deriving (Eq, Show, Typeable)

typeInt :: IntegralType Int
typeInt = TypeInt (IntegralDict @Int)

typeFloat :: FloatingType Float
typeFloat = TypeFloat (FloatingDict @Float)

numTypeInt :: NumType Int
numTypeInt = IntegralNumType typeInt

numTypeFloat :: NumType Float
numTypeFloat = FloatingNumType typeFloat

singleTypeInt :: SingleType Int
singleTypeInt = NumSingleType numTypeInt

singleTypeLabel :: SingleType Label
singleTypeLabel = LabelSingleType

singleTypeUnionInt :: SingleType (Either Int Int)
singleTypeUnionInt = UnionSingleType singleTypeInt singleTypeInt

-- stypeToTypeRep :: SingleType a -> TypeRep
-- stypeToTypeRep LabelSingleType = typeOf (undefined :: Label)
-- stypeToTypeRep UnitSingleType = typeOf (undefined :: ())
-- stypeToTypeRep (UnionSingleType (_ :: SingleType a) (_ :: SingleType b)) = typeOf (undefined :: (Either a b))
-- stypeToTypeRep (NumSingleType (_ :: NumType a)) = typeOf (undefined :: a)

class Typeable a => Repr a where
    singleType :: SingleType a

instance Repr () where
    singleType = UnitSingleType

instance Repr Int where
    singleType = singleTypeInt

instance Repr Float where
    singleType = NumSingleType numTypeFloat

instance (Repr a, Repr b) => Repr (Either a b) where
    singleType = UnionSingleType singleType singleType

instance (Repr a, Repr b) => Repr (a, b) where
    singleType = ProductSingleType singleType singleType