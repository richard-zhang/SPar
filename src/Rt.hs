{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Rt where

import           Data.Type.Equality
import           Data.Proxy
import           Data.Either
import           Data.Singletons.TypeLits
import           Data.Singletons
import           Control.Monad.Free
import           Data.Kind
import qualified Data.Map.Strict               as Map
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           GHC.Natural
import           Data.Typeable
import           Language.Poly.Core             ( Core(Lit)
                                                , Serialise
                                                , interp
                                                )
import           Def                     hiding ( (>>=)
                                                , (>>)
                                                , return
                                                )
import           Type
import qualified Control.Monad.Indexed.Free    as F

type ProcessRT a = (ProcRT a, Natural)

data ProcRTF next where
    Send' :: (Serialise a) => Natural -> Core a -> next -> ProcRTF next
    Recv' :: (Serialise a) => Natural -> (Core a -> next) -> ProcRTF next
    Select' :: (Serialise a, Serialise b, Serialise c) => Natural -> Core (Either a b) -> (Core a -> ProcRT c) -> (Core b -> ProcRT c) -> next -> ProcRTF next
    Branch' :: (Serialise c) => Natural -> ProcRT c -> ProcRT c -> next -> ProcRTF next

instance Functor ProcRTF where
    fmap f (Send' r v n) = Send' r v $ f n
    fmap f (Recv' r cont) = Recv' r (f . cont)
    fmap f (Select' r v cont1 cont2 n) = Select' r v cont1 cont2 (f n)
    fmap f (Branch' r p1 p2 n) = Branch' r p1 p2 (f n)

type ProcRT a = Free ProcRTF (Core a)

data Label = L | R deriving (Show)

type GlobalMq = Map.Map Natural (Map.Map Natural [Either String Label])
type Gs = StateT GlobalMq (WriterT [ObservableAction] IO) ()

data ObservableAction =
    ASend Natural Natural String
  | ARecv Natural Natural String
  | ASelect
  | ABranch
  deriving (Show)

eraseSessionInfo' :: Proc' i j a -> ProcRT a
eraseSessionInfo' (F.Pure v) = Pure v
eraseSessionInfo' (F.Free (Send (r :: Sing (n :: Nat)) v next)) =
    Free (Send' (fromSing r) v (eraseSessionInfo' next))
eraseSessionInfo' (F.Free (Recv (r :: Sing (n :: Nat)) cont)) =
    Free (Recv' (fromSing r) (eraseSessionInfo' . cont))
eraseSessionInfo' (F.Free (Select (r :: Sing (n :: Nat)) v cont1 cont2 next)) =
    Free
        (Select' (fromSing r)
                 v
                 (eraseSessionInfo' . cont1)
                 (eraseSessionInfo' . cont2)
                 (eraseSessionInfo' next)
        )
eraseSessionInfo' (F.Free (Branch (r :: Sing (n :: Nat)) left right next)) =
    Free
        (Branch' (fromSing r)
                 (eraseSessionInfo' left)
                 (eraseSessionInfo' right)
                 (eraseSessionInfo' next)
        )

eraseSessionInfo :: Process k a -> ProcessRT a
eraseSessionInfo (Process n value) = (eraseSessionInfo' value, fromSing n)

serialize :: (Serialise a) => Core a -> String
serialize (Lit a) = show a

deserialize :: (Serialise a) => String -> Core a
deserialize = Lit . read

normalEval :: [ProcessRT ()] -> Gs
normalEval []       = return ()
normalEval (x : xs) = normalEval' x xs

normalEval' :: ProcessRT () -> [ProcessRT ()] -> Gs
normalEval' (Pure _                       , role) xs = normalEval xs
normalEval' (Free (Send' receiver value n), role) xs = do
    env <- get
    debug (return env)
    put
        (Map.update
            (Just . Map.insertWith (flip (++)) role [Left $ serialize value])
            receiver
            env
        )
    debug get
    tell [ASend receiver role (serialize value)]
    normalEval (xs ++ [(n, role)])
normalEval' act@(Free (Recv' sender cont), role) xs = do
    env <- get
    let value = Map.lookup sender =<< Map.lookup role env
    case value of
        Just (Left x : rest) -> do
            put
                (Map.update (Just . Map.update (const $ Just rest) sender)
                            role
                            env
                )
            tell [ARecv sender role x]
            normalEval (xs ++ [(cont $ deserialize x, role)])
        Just [] -> normalEval $ xs ++ [act]
        Nothing -> error "u"
normalEval' (Free (Select' receiver choice left right next), role) xs = do
    let value = interp choice
    let label = if isLeft value then Rt.L else Rt.R
    env <- get
    put
        (Map.update (Just . Map.insertWith (flip (++)) role [Right label])
                    receiver
                    env
        )
    case value of
        Left a ->
            tell [ASelect] >> normalEval (xs ++ [(left (Lit a) >> next, role)])
        Right b ->
            tell [ASelect] >> normalEval (xs ++ [(right (Lit b) >> next, role)])
normalEval' act@(Free (Branch' sender left right next), role) xs = do
    env <- get
    let value = Map.lookup sender =<< Map.lookup role env
    case value of
        Just (Right x : rest) -> do
            put
                (Map.update (Just . Map.update (const $ Just rest) sender)
                            role
                            env
                )
            tell [ABranch]
            normalEval
                (  xs
                ++ [ ( (case x of
                           Rt.L -> left
                           Rt.R -> right
                       )
                         >> next
                     , role
                     )
                   ]
                )
        Just [] -> normalEval $ xs ++ [act]
        Nothing -> error "br"

eval' :: [ProcessRT ()] -> IO [ObservableAction]
eval' xs = snd <$> runWriterT (runStateT (normalEval xs) (initialEnv xs))

evalN :: DualityCons xs => PList xs -> IO [ObservableAction]
evalN = eval' . convert2Normal
  where
    convert2Normal :: PList xs -> [ProcessRT ()]
    convert2Normal PNil         = []
    convert2Normal (PCons p ps) = eraseSessionInfo p : convert2Normal ps

initialEnv :: [ProcessRT ()] -> GlobalMq
initialEnv = Map.fromList . fmap (\(_, r) -> (r, Map.empty))

debug :: (MonadIO m, Show a) => m a -> m ()
debug = (>>= liftIO . print)