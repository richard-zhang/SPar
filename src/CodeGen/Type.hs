{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
module CodeGen.Type where

import           Data.Bits
import           Data.Typeable
import           Data.List
import           Foreign.Storable               ( Storable )

type Serialise a = (Repr a, Show a, Read a)

-- need to find a way to represent recursive single type 
data SingleType a where
    NumSingleType :: NumType a -> SingleType a
    LabelSingleType :: SingleType Label
    SumSingleType :: (Typeable a, Typeable b) => SingleType a -> SingleType b -> SingleType (Either a b)
    UnitSingleType :: SingleType ()
    ProductSingleType :: (Typeable a, Typeable b) => SingleType a -> SingleType b -> SingleType (a, b)
    ListSingleType :: Typeable a => SingleType a -> SingleType [a]

data ASingleType where
    ASingleType :: forall a. SingleType a -> ASingleType

equal :: SingleType a -> SingleType b -> Bool
equal (NumSingleType (IntegralNumType _)) (NumSingleType (IntegralNumType _)) =
    True
equal (NumSingleType (FloatingNumType _)) (NumSingleType (FloatingNumType _)) =
    True
equal LabelSingleType     LabelSingleType       = True
equal (SumSingleType a b) (SumSingleType a' b') = equal a a' && equal b b'
equal (ProductSingleType a b) (ProductSingleType a' b') =
    equal a a' && equal b b'
equal UnitSingleType       UnitSingleType         = True
equal (ListSingleType a  ) (ListSingleType b    ) = a `equal` b
equal _                    _                      = False

sTypeHeight :: SingleType a -> Int
sTypeHeight (NumSingleType _)       = 0
sTypeHeight LabelSingleType         = 0
sTypeHeight (SumSingleType     a b) = 1 + max (sTypeHeight a) (sTypeHeight b)
sTypeHeight (ProductSingleType a b) = 1 + max (sTypeHeight a) (sTypeHeight b)
sTypeHeight (UnitSingleType       ) = 0
sTypeHeight (ListSingleType a     ) = 1 + sTypeHeight a

compareSingleType :: SingleType a -> SingleType b -> Ordering
compareSingleType a b
    | a `equal` b = EQ
    | otherwise = case (sTypeHeight a) `compare` (sTypeHeight b) of
        EQ -> LT
        x  -> x

instance Show (SingleType a) where
    show (NumSingleType (IntegralNumType _)) = "int"
    show (NumSingleType (FloatingNumType _)) = "float"
    show UnitSingleType                      = "unit"
    show LabelSingleType                     = "Label"
    show (SumSingleType a b) = intercalate "_" ["Sum", show a, show b]
    show (ProductSingleType a b) = intercalate "_" ["Prod", show a, show b]
    show (ListSingleType a     )             = intercalate "_" ["List", show a]

instance Show ASingleType where
    show (ASingleType s) = show s

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
singleTypeUnionInt = SumSingleType singleTypeInt singleTypeInt

class Typeable a => Repr a where
    singleType :: SingleType a

instance Repr () where
    singleType = UnitSingleType

instance Repr Int where
    singleType = singleTypeInt

instance Repr Label where
    singleType = LabelSingleType

instance Repr Float where
    singleType = NumSingleType numTypeFloat

instance (Repr a, Repr b) => Repr (Either a b) where
    singleType = SumSingleType singleType singleType

instance (Repr a, Repr b) => Repr (a, b) where
    singleType = ProductSingleType singleType singleType

instance Repr a => Repr [a] where
    singleType = ListSingleType singleType

-- instance (Repr a, Repr b) => Repr (a -> b) where
--     singleType = FuncSingleType singleType singleType

-- ty1 = ListSingleType (singleType :: SingleType Int)
-- ty2 = ProductSingleType ty1 ty1
-- ty3 = SumSingleType UnitSingleType (singleType :: SingleType Int)
-- ty4 = SumSingleType ty3 ty2

-- ty1' = toASingleType ty1
-- ty2' = toASingleType ty2
-- ty3' = toASingleType ty3
-- ty4' = toASingleType ty4

-- ts = [ty1', ty2', ty3', ty4']
