{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ExistentialQuantification #-}
module CodeGen.Type where

import           Data.Bits
import           Data.Typeable
import           Data.List
import           Foreign.Storable               ( Storable )

-- need to find a way to represent recursive single type 
data SingleType a where
    NumSingleType :: NumType a -> SingleType a
    LabelSingleType :: SingleType Label
    UnionSingleType :: (Typeable a, Typeable b) => SingleType a -> SingleType b -> SingleType (Either a b)
    UnitSingleType :: SingleType ()
    ProductSingleType :: (Typeable a, Typeable b) => SingleType a -> SingleType b -> SingleType (a, b)
    ListSingleType :: Typeable a => SingleType a -> SingleType [a]
    FuncSingleType :: (Typeable a, Typeable b) => SingleType a -> SingleType b -> SingleType (a -> b)

data ASingleType where
    ASingleType :: forall a. SingleType a -> ASingleType

equal :: SingleType a -> SingleType b -> Bool
equal (NumSingleType (IntegralNumType _)) (NumSingleType (IntegralNumType _)) =
    True
equal (NumSingleType (FloatingNumType _)) (NumSingleType (FloatingNumType _)) =
    True
equal LabelSingleType       LabelSingleType         = True
equal (UnionSingleType a b) (UnionSingleType a' b') = equal a a' && equal b b'
equal (ProductSingleType a b) (UnionSingleType a' b') =
    equal a a' && equal b b'
equal UnitSingleType UnitSingleType = True
equal _              _              = False

sTypeHeight :: SingleType a -> Int
sTypeHeight (NumSingleType _)       = 0
sTypeHeight LabelSingleType         = 0
sTypeHeight (UnionSingleType   a b) = 1 + max (sTypeHeight a) (sTypeHeight b)
sTypeHeight (ProductSingleType a b) = 1 + max (sTypeHeight a) (sTypeHeight b)
sTypeHeight (UnitSingleType       ) = 0

compareSingleType :: SingleType a -> SingleType b -> Ordering
compareSingleType a b | a `equal` b = EQ
                      | otherwise   = (sTypeHeight a) `compare` (sTypeHeight b)

instance Show (SingleType a) where
    show (NumSingleType (IntegralNumType _)) = "int"
    show (NumSingleType (FloatingNumType _)) = "float"
    show (UnionSingleType a b) = intercalate "_" ["Sum", show a, show b]
    show (ProductSingleType a b) = intercalate "_" ["Prod", show a, show b]
    show UnitSingleType = "unit"
    show LabelSingleType = "Label"

toASingleType :: SingleType a -> ASingleType
toASingleType stype = ASingleType stype

instance Eq (SingleType a) where
    (==) = equal

instance Ord (SingleType a) where
    compare a b = compareSingleType a b

instance Eq (ASingleType) where
    (ASingleType left) == (ASingleType right) = equal left right

instance Ord ASingleType where
    compare (ASingleType left) (ASingleType right) =
        compareSingleType left right

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

instance Repr a => Repr [a] where
    singleType = ListSingleType singleType

instance (Repr a, Repr b) => Repr (a -> b) where
    singleType = FuncSingleType singleType singleType