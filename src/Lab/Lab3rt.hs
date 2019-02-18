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
module Lab.Lab3rt where

import           Prelude
import           Data.Type.Equality
import           Data.Proxy
import           Data.Singletons.TypeLits
import           Data.Singletons
import           Control.Monad.Free
import           Data.Kind
import qualified Data.Map.Strict               as Map
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Lab.Lab3                hiding ( IxMonad(..) )
import           GHC.Natural
import           Data.Typeable

type GlobalMq = Map.Map Natural [String]
type Gs = StateT GlobalMq (WriterT [ObservableAction] IO) ()

serialize :: (CC a) => Expr a -> String
serialize (Var x) = show x

deserialize :: (CC a) => String -> Expr a
deserialize a = Var $ read a

data ObservableAction =
    ASend Natural Natural String
  | ARecv Natural Natural String
  deriving (Show)

data Pf' next where
    Send' :: (CC a) => Natural -> Expr a -> next -> Pf' next
    Recv' :: (CC a) => Natural -> (Expr a -> next) -> Pf' next

instance Functor Pf' where
    fmap f (Send' r v n) = Send' r v $ f n
    fmap f (Recv' r cont) = Recv' r (f . cont)

type Process' a = (Natural, P' a)
type P' a = Free Pf' (Expr a)

eraseSessionInfo' :: P i a -> P' a
eraseSessionInfo' (Return v) = Pure v
eraseSessionInfo' (Wrap (Send (r :: Sing (n :: Nat)) v next)) =
    Free (Send' (fromSing r) v (eraseSessionInfo' next))
eraseSessionInfo' (Wrap (Recv (r :: Sing (n :: Nat)) cont)) =
    Free (Recv' (fromSing r) (eraseSessionInfo' . cont))

eraseSessionInfo :: Process k a -> Process' a
eraseSessionInfo (Process n value) = (fromSing n, eraseSessionInfo' value)

normalEval :: [Process' ()] -> Gs
normalEval []       = return ()
normalEval (x : xs) = normalEval' x xs

normalEval' :: Process' () -> [Process' ()] -> Gs
normalEval' (role, Pure _                       ) xs = normalEval xs
normalEval' (role, Free (Send' receiver value n)) xs = do
    env <- get
    -- debug (return env)
    put (Map.update (Just . (++ [serialize value])) receiver env)
    -- debug get
    tell [ASend receiver role (serialize value)]
    normalEval (xs ++ [(role, n)])
normalEval' act@(role, Free (Recv' sender cont)) xs = do
    env <- get
    let value = Map.lookup role env
    case value of
        Just (x : rest) -> do
            put (Map.update (const (Just rest)) role env)
            tell [ARecv sender role x]
            normalEval (xs ++ [(role, cont $ deserialize x)])
        Just [] -> normalEval $ xs ++ [act]
        Nothing -> error "u"

eval' :: [Process' ()] -> IO [ObservableAction]
eval' xs = snd <$> runWriterT (runStateT (normalEval xs) (initialEnv xs))

evalN :: DualityCons xs => PList xs -> IO [ObservableAction]
evalN = eval' . convert2Normal
  where
    convert2Normal :: PList xs -> [Process' ()]
    convert2Normal PNil         = []
    convert2Normal (PCons p ps) = eraseSessionInfo p : convert2Normal ps

initialEnv :: [Process' ()] -> GlobalMq
initialEnv = Map.fromList . fmap (\(r, _) -> (r, []))

debug :: (MonadIO m, Show a) => m a -> m ()
debug x = x >>= liftIO . print