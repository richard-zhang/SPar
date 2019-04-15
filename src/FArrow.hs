{-# LANGUAGE MultiParamTypeClasses #-}

module FArrow where

import Control.Arrow (Arrow, ArrowChoice)
import qualified Control.Arrow
import Control.Category (Category)
import qualified Control.Category
import Prelude hiding (id, (.))

--infixr 9 .
--
--infixr 1 >>>, <<<
--
--infixr 3 ***
--
--infixr 3 &&&
--
--infixr 2 +++
--
--infixr 2 |||

-- class FCategory cat f where
--   id :: cat f a a
--   (.) :: cat f b c -> cat f a b -> cat f a c

-- instance (Functor f, FCategory cat f) => Category (cat f) where
--   id = id
--   (.) = (.)

-- (<<<) :: FCategory cat f => cat f b c -> cat f a b -> cat f a c
-- (<<<) = (.)

-- (>>>) :: FCategory cat f => cat f a b -> cat f b c -> cat f a c
-- f >>> g = g . f

-- class FCategory a f =>
--       FArrow a f
--   where
--   first :: a f b c -> a f (b, d) (c, d)
--   second :: a f b c -> a f (d, b) (d, c)
--   (***) :: a f b c -> a f b' c' -> a f (b, b') (c, c')
--   (&&&) :: a f b c -> a f b c' -> a f b (c, c')

--   swap :: f (b, c) -> f (c, b)

--class FCategory a =>
--      FArrow a
--  where
--  arr :: (f b -> f c) -> a f b c
--  first :: a f b c -> a f (b, d) (c, d)
--  first = (*** id)
--  second :: a f b c -> a f (d, b) (d, c)
--  second = (id ***)
--  (***) :: a f b c -> a f b' c' -> a f (b, b') (c, c')
--  (&&&) :: a f b c -> a f b c' -> a f b (c, c')
--
--instance (FArrow cat, Functor f) => Arrow (cat f) where
--  arr f = FArrow.arr $ fmap f
--  first = FArrow.first
--
--class FArrow a =>
--      FArrowChoice a
--  where
--  left :: a f b c -> a f (Either b d) (Either c d)
--  right :: a f b c -> a f (Either d b) (Either d c)
--  (+++) :: a f b c -> a f b' c' -> a f (Either b b') (Either c c')
--  (|||) :: a f b d -> a f c d -> a f (Either b c) d
--
--instance (FArrowChoice cat, Functor f) => ArrowChoice (cat f) where
--  left = left
--
--newtype FKleisli m f a b = FKleisli { runFKleisli :: f a -> m (f b) }
--
--instance Monad m => FCategory (FKleisli m) where
--  id = FKleisli return
--  (FKleisli f) . (FKleisli g) = FKleisli (\a -> g a >>= f)
