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
module Rt2 where

import           Control.Monad.Free
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Either
import           Data.List                      ( intercalate )
import qualified Data.Map.Strict               as Map
import           Data.Type.Natural              ( Nat )
import           Def                     hiding ( return
                                                , (>>)
                                                , (>>=)
                                                )
import           Language.Poly.Core
import           RtDef
import           TypeValue

data InterpState = InterpState
    {
        outputValues :: [(Nat, String)],
        trace :: [String],
        messageQueue :: [(Nat, Nat, String)]
    }

updateOutputValue :: Nat -> a -> State InterpState ()
updateOutputValue = undefined

updateMessageQueue :: (Nat, Nat, String) -> State InterpState ()
updateMessageQueue = undefined

updateTrace :: String -> State InterpState ()
updateTrace = undefined

getTopMessage :: State InterpState (Nat, Nat, String)
getTopMessage = undefined

popMessageQueue :: State InterpState ()
popMessageQueue = undefined

interpCore :: Core a -> a
interpCore = undefined

interpret :: [(ProcRT (), Nat)] -> State InterpState ()
interpret []       = return ()
interpret (x : xs) = interp' x xs

interp' :: (ProcRT (), Nat) -> [(ProcRT (), Nat)] -> State InterpState ()
interp' (Pure value, role) xs = do
    updateOutputValue role value
    interpret xs
interp' (Free (Send' receiver value next), role) xs = do
    updateMessageQueue (role, receiver, show $ interpCore value)
    updateTrace "send"
    interpret $ xs ++ [(next, role)]
interp' p@(Free (Recv' sender cont), role) xs = do
    (x, y, v) <- getTopMessage
    if x == sender && y == role
        then do
            popMessageQueue
            updateTrace "recv"
            let value = Lit (read v)
            interpret $ xs ++ [(cont value, role)]
        else interpret $ xs ++ [p]
