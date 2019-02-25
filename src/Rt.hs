{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Rt where

import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Either
import qualified Data.Map.Strict as Map
import Data.Type.Natural (Nat)
import Def hiding (return, (>>), (>>=))
import Language.Poly.Core
import RtDef
import TypeValue

data Label = Le | Ri deriving (Show)

type GlobalMq = Map.Map Nat (Map.Map Nat [Either String Label])
type Gs = StateT GlobalMq (WriterT [ObservableAction] IO) ()

data ObservableAction =
    ASend Nat Nat String
  | ARecv Nat Nat String
  | ASelect
  | ABranch
  deriving (Show)

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
    let label = if isLeft value then Le else Ri
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
                           Le -> left
                           Ri -> right
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

evalN2 :: [ProcessRT ()] -> IO [ObservableAction]
evalN2 xs = if dualityC (fmap (\(a, b) -> (convert 0 a, b)) xs)
    then eval' xs
    else error "Not dual"

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
